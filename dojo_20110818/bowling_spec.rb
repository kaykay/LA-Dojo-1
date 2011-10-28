$: << "."
require 'bowling_scorer'

describe BowlingScorer do
  it "scores a single roll" do
    subject.roll(8)
    subject.total_score.should == 8
  end

  it "scores a spare" do
    subject.roll(8)
    subject.roll(2)
    subject.roll(6)

    subject.total_score.should == 22
  end
  
end
