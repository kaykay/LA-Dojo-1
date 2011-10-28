class BowlingScorer

  attr_reader :total_score
  def initialize
    @total_score = 0
  end

  def roll(points)
    @total_score += points
  end


end