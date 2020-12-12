{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.TimeWindow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.TimeWindow
  ( TimeWindow (..),

    -- * Smart constructor
    mkTimeWindow,

    -- * Lenses
    twStartTime,
    twEndTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | In a 'GetSampledRequests' request, the @StartTime@ and @EndTime@ objects specify the time range for which you want AWS WAF to return a sample of web requests.
--
-- You must specify the times in Coordinated Universal Time (UTC) format. UTC format includes the special designator, @Z@ . For example, @"2016-09-27T14:50Z"@ .
-- In a 'GetSampledRequests' response, the @StartTime@ and @EndTime@ objects specify the time range for which AWS WAF actually returned a sample of web requests. AWS WAF gets the specified number of requests from among the first 5,000 requests that your AWS resource receives during the specified time period. If your resource receives more than 5,000 requests during that period, AWS WAF stops sampling after the 5,000th request. In that case, @EndTime@ is the time that AWS WAF received the 5,000th request.
--
-- /See:/ 'mkTimeWindow' smart constructor.
data TimeWindow = TimeWindow'
  { startTime :: Lude.Timestamp,
    endTime :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TimeWindow' with the minimum fields required to make a request.
--
-- * 'endTime' - The end of the time range from which you want @GetSampledRequests@ to return a sample of the requests that your AWS resource received. You must specify the date and time in Coordinated Universal Time (UTC) format. UTC format includes the special designator, @Z@ . For example, @"2016-09-27T14:50Z"@ . You can specify any time range in the previous three hours.
-- * 'startTime' - The beginning of the time range from which you want @GetSampledRequests@ to return a sample of the requests that your AWS resource received. You must specify the date and time in Coordinated Universal Time (UTC) format. UTC format includes the special designator, @Z@ . For example, @"2016-09-27T14:50Z"@ . You can specify any time range in the previous three hours.
mkTimeWindow ::
  -- | 'startTime'
  Lude.Timestamp ->
  -- | 'endTime'
  Lude.Timestamp ->
  TimeWindow
mkTimeWindow pStartTime_ pEndTime_ =
  TimeWindow' {startTime = pStartTime_, endTime = pEndTime_}

-- | The beginning of the time range from which you want @GetSampledRequests@ to return a sample of the requests that your AWS resource received. You must specify the date and time in Coordinated Universal Time (UTC) format. UTC format includes the special designator, @Z@ . For example, @"2016-09-27T14:50Z"@ . You can specify any time range in the previous three hours.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twStartTime :: Lens.Lens' TimeWindow Lude.Timestamp
twStartTime = Lens.lens (startTime :: TimeWindow -> Lude.Timestamp) (\s a -> s {startTime = a} :: TimeWindow)
{-# DEPRECATED twStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The end of the time range from which you want @GetSampledRequests@ to return a sample of the requests that your AWS resource received. You must specify the date and time in Coordinated Universal Time (UTC) format. UTC format includes the special designator, @Z@ . For example, @"2016-09-27T14:50Z"@ . You can specify any time range in the previous three hours.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
twEndTime :: Lens.Lens' TimeWindow Lude.Timestamp
twEndTime = Lens.lens (endTime :: TimeWindow -> Lude.Timestamp) (\s a -> s {endTime = a} :: TimeWindow)
{-# DEPRECATED twEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

instance Lude.FromJSON TimeWindow where
  parseJSON =
    Lude.withObject
      "TimeWindow"
      ( \x ->
          TimeWindow'
            Lude.<$> (x Lude..: "StartTime") Lude.<*> (x Lude..: "EndTime")
      )

instance Lude.ToJSON TimeWindow where
  toJSON TimeWindow' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("StartTime" Lude..= startTime),
            Lude.Just ("EndTime" Lude..= endTime)
          ]
      )
