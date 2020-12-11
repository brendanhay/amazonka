-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.ErrorStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.ErrorStatistics
  ( ErrorStatistics (..),

    -- * Smart constructor
    mkErrorStatistics,

    -- * Lenses
    eOtherCount,
    eThrottleCount,
    eTotalCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about requests that failed with a 4xx Client Error status code.
--
-- /See:/ 'mkErrorStatistics' smart constructor.
data ErrorStatistics = ErrorStatistics'
  { otherCount ::
      Lude.Maybe Lude.Integer,
    throttleCount :: Lude.Maybe Lude.Integer,
    totalCount :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ErrorStatistics' with the minimum fields required to make a request.
--
-- * 'otherCount' - The number of requests that failed with untracked 4xx Client Error status codes.
-- * 'throttleCount' - The number of requests that failed with a 419 throttling status code.
-- * 'totalCount' - The total number of requests that failed with a 4xx Client Error status code.
mkErrorStatistics ::
  ErrorStatistics
mkErrorStatistics =
  ErrorStatistics'
    { otherCount = Lude.Nothing,
      throttleCount = Lude.Nothing,
      totalCount = Lude.Nothing
    }

-- | The number of requests that failed with untracked 4xx Client Error status codes.
--
-- /Note:/ Consider using 'otherCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eOtherCount :: Lens.Lens' ErrorStatistics (Lude.Maybe Lude.Integer)
eOtherCount = Lens.lens (otherCount :: ErrorStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {otherCount = a} :: ErrorStatistics)
{-# DEPRECATED eOtherCount "Use generic-lens or generic-optics with 'otherCount' instead." #-}

-- | The number of requests that failed with a 419 throttling status code.
--
-- /Note:/ Consider using 'throttleCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eThrottleCount :: Lens.Lens' ErrorStatistics (Lude.Maybe Lude.Integer)
eThrottleCount = Lens.lens (throttleCount :: ErrorStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {throttleCount = a} :: ErrorStatistics)
{-# DEPRECATED eThrottleCount "Use generic-lens or generic-optics with 'throttleCount' instead." #-}

-- | The total number of requests that failed with a 4xx Client Error status code.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eTotalCount :: Lens.Lens' ErrorStatistics (Lude.Maybe Lude.Integer)
eTotalCount = Lens.lens (totalCount :: ErrorStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {totalCount = a} :: ErrorStatistics)
{-# DEPRECATED eTotalCount "Use generic-lens or generic-optics with 'totalCount' instead." #-}

instance Lude.FromJSON ErrorStatistics where
  parseJSON =
    Lude.withObject
      "ErrorStatistics"
      ( \x ->
          ErrorStatistics'
            Lude.<$> (x Lude..:? "OtherCount")
            Lude.<*> (x Lude..:? "ThrottleCount")
            Lude.<*> (x Lude..:? "TotalCount")
      )
