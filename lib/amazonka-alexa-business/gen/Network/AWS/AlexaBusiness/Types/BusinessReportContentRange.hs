{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportContentRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReportContentRange
  ( BusinessReportContentRange (..),

    -- * Smart constructor
    mkBusinessReportContentRange,

    -- * Lenses
    brcrInterval,
  )
where

import Network.AWS.AlexaBusiness.Types.BusinessReportInterval
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The content range of the report.
--
-- /See:/ 'mkBusinessReportContentRange' smart constructor.
newtype BusinessReportContentRange = BusinessReportContentRange'
  { -- | The interval of the content range.
    interval :: BusinessReportInterval
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BusinessReportContentRange' with the minimum fields required to make a request.
--
-- * 'interval' - The interval of the content range.
mkBusinessReportContentRange ::
  -- | 'interval'
  BusinessReportInterval ->
  BusinessReportContentRange
mkBusinessReportContentRange pInterval_ =
  BusinessReportContentRange' {interval = pInterval_}

-- | The interval of the content range.
--
-- /Note:/ Consider using 'interval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brcrInterval :: Lens.Lens' BusinessReportContentRange BusinessReportInterval
brcrInterval = Lens.lens (interval :: BusinessReportContentRange -> BusinessReportInterval) (\s a -> s {interval = a} :: BusinessReportContentRange)
{-# DEPRECATED brcrInterval "Use generic-lens or generic-optics with 'interval' instead." #-}

instance Lude.FromJSON BusinessReportContentRange where
  parseJSON =
    Lude.withObject
      "BusinessReportContentRange"
      ( \x ->
          BusinessReportContentRange' Lude.<$> (x Lude..: "Interval")
      )

instance Lude.ToJSON BusinessReportContentRange where
  toJSON BusinessReportContentRange' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Interval" Lude..= interval)])
