{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.ReportFilter
  ( ReportFilter (..)
  -- * Smart constructor
  , mkReportFilter
  -- * Lenses
  , rfStatus
  ) where

import qualified Network.AWS.CodeBuild.Types.ReportStatusType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A filter used to return reports with the status specified by the input @status@ parameter. 
--
-- /See:/ 'mkReportFilter' smart constructor.
newtype ReportFilter = ReportFilter'
  { status :: Core.Maybe Types.ReportStatusType
    -- ^ The status used to filter reports. You can filter using one status only. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ReportFilter' value with any optional fields omitted.
mkReportFilter
    :: ReportFilter
mkReportFilter = ReportFilter'{status = Core.Nothing}

-- | The status used to filter reports. You can filter using one status only. 
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfStatus :: Lens.Lens' ReportFilter (Core.Maybe Types.ReportStatusType)
rfStatus = Lens.field @"status"
{-# INLINEABLE rfStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON ReportFilter where
        toJSON ReportFilter{..}
          = Core.object (Core.catMaybes [("status" Core..=) Core.<$> status])
