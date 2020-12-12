{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.FaultStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.FaultStatistics
  ( FaultStatistics (..),

    -- * Smart constructor
    mkFaultStatistics,

    -- * Lenses
    fsOtherCount,
    fsTotalCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about requests that failed with a 5xx Server Error status code.
--
-- /See:/ 'mkFaultStatistics' smart constructor.
data FaultStatistics = FaultStatistics'
  { otherCount ::
      Lude.Maybe Lude.Integer,
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

-- | Creates a value of 'FaultStatistics' with the minimum fields required to make a request.
--
-- * 'otherCount' - The number of requests that failed with untracked 5xx Server Error status codes.
-- * 'totalCount' - The total number of requests that failed with a 5xx Server Error status code.
mkFaultStatistics ::
  FaultStatistics
mkFaultStatistics =
  FaultStatistics'
    { otherCount = Lude.Nothing,
      totalCount = Lude.Nothing
    }

-- | The number of requests that failed with untracked 5xx Server Error status codes.
--
-- /Note:/ Consider using 'otherCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsOtherCount :: Lens.Lens' FaultStatistics (Lude.Maybe Lude.Integer)
fsOtherCount = Lens.lens (otherCount :: FaultStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {otherCount = a} :: FaultStatistics)
{-# DEPRECATED fsOtherCount "Use generic-lens or generic-optics with 'otherCount' instead." #-}

-- | The total number of requests that failed with a 5xx Server Error status code.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fsTotalCount :: Lens.Lens' FaultStatistics (Lude.Maybe Lude.Integer)
fsTotalCount = Lens.lens (totalCount :: FaultStatistics -> Lude.Maybe Lude.Integer) (\s a -> s {totalCount = a} :: FaultStatistics)
{-# DEPRECATED fsTotalCount "Use generic-lens or generic-optics with 'totalCount' instead." #-}

instance Lude.FromJSON FaultStatistics where
  parseJSON =
    Lude.withObject
      "FaultStatistics"
      ( \x ->
          FaultStatistics'
            Lude.<$> (x Lude..:? "OtherCount") Lude.<*> (x Lude..:? "TotalCount")
      )
