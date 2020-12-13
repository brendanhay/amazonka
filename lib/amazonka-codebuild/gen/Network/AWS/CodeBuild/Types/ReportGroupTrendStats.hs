{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportGroupTrendStats
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportGroupTrendStats
  ( ReportGroupTrendStats (..),

    -- * Smart constructor
    mkReportGroupTrendStats,

    -- * Lenses
    rgtsMax,
    rgtsAverage,
    rgtsMin,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkReportGroupTrendStats' smart constructor.
data ReportGroupTrendStats = ReportGroupTrendStats'
  { max :: Lude.Maybe Lude.Text,
    average :: Lude.Maybe Lude.Text,
    min :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReportGroupTrendStats' with the minimum fields required to make a request.
--
-- * 'max' -
-- * 'average' -
-- * 'min' -
mkReportGroupTrendStats ::
  ReportGroupTrendStats
mkReportGroupTrendStats =
  ReportGroupTrendStats'
    { max = Lude.Nothing,
      average = Lude.Nothing,
      min = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'max' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgtsMax :: Lens.Lens' ReportGroupTrendStats (Lude.Maybe Lude.Text)
rgtsMax = Lens.lens (max :: ReportGroupTrendStats -> Lude.Maybe Lude.Text) (\s a -> s {max = a} :: ReportGroupTrendStats)
{-# DEPRECATED rgtsMax "Use generic-lens or generic-optics with 'max' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'average' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgtsAverage :: Lens.Lens' ReportGroupTrendStats (Lude.Maybe Lude.Text)
rgtsAverage = Lens.lens (average :: ReportGroupTrendStats -> Lude.Maybe Lude.Text) (\s a -> s {average = a} :: ReportGroupTrendStats)
{-# DEPRECATED rgtsAverage "Use generic-lens or generic-optics with 'average' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'min' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgtsMin :: Lens.Lens' ReportGroupTrendStats (Lude.Maybe Lude.Text)
rgtsMin = Lens.lens (min :: ReportGroupTrendStats -> Lude.Maybe Lude.Text) (\s a -> s {min = a} :: ReportGroupTrendStats)
{-# DEPRECATED rgtsMin "Use generic-lens or generic-optics with 'min' instead." #-}

instance Lude.FromJSON ReportGroupTrendStats where
  parseJSON =
    Lude.withObject
      "ReportGroupTrendStats"
      ( \x ->
          ReportGroupTrendStats'
            Lude.<$> (x Lude..:? "max")
            Lude.<*> (x Lude..:? "average")
            Lude.<*> (x Lude..:? "min")
      )
