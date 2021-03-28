{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportWithRawData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.ReportWithRawData
  ( ReportWithRawData (..)
  -- * Smart constructor
  , mkReportWithRawData
  -- * Lenses
  , rwrdData
  , rwrdReportArn
  ) where

import qualified Network.AWS.CodeBuild.Types.NonEmptyString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkReportWithRawData' smart constructor.
data ReportWithRawData = ReportWithRawData'
  { data' :: Core.Maybe Core.Text
  , reportArn :: Core.Maybe Types.NonEmptyString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReportWithRawData' value with any optional fields omitted.
mkReportWithRawData
    :: ReportWithRawData
mkReportWithRawData
  = ReportWithRawData'{data' = Core.Nothing,
                       reportArn = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwrdData :: Lens.Lens' ReportWithRawData (Core.Maybe Core.Text)
rwrdData = Lens.field @"data'"
{-# INLINEABLE rwrdData #-}
{-# DEPRECATED data' "Use generic-lens or generic-optics with 'data'' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'reportArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rwrdReportArn :: Lens.Lens' ReportWithRawData (Core.Maybe Types.NonEmptyString)
rwrdReportArn = Lens.field @"reportArn"
{-# INLINEABLE rwrdReportArn #-}
{-# DEPRECATED reportArn "Use generic-lens or generic-optics with 'reportArn' instead"  #-}

instance Core.FromJSON ReportWithRawData where
        parseJSON
          = Core.withObject "ReportWithRawData" Core.$
              \ x ->
                ReportWithRawData' Core.<$>
                  (x Core..:? "data") Core.<*> x Core..:? "reportArn"
