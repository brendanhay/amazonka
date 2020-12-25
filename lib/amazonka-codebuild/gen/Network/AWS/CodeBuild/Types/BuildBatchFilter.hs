{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildBatchFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildBatchFilter
  ( BuildBatchFilter (..),

    -- * Smart constructor
    mkBuildBatchFilter,

    -- * Lenses
    bbfStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types.StatusType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies filters when retrieving batch builds.
--
-- /See:/ 'mkBuildBatchFilter' smart constructor.
newtype BuildBatchFilter = BuildBatchFilter'
  { -- | The status of the batch builds to retrieve. Only batch builds that have this status will be retrieved.
    status :: Core.Maybe Types.StatusType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BuildBatchFilter' value with any optional fields omitted.
mkBuildBatchFilter ::
  BuildBatchFilter
mkBuildBatchFilter = BuildBatchFilter' {status = Core.Nothing}

-- | The status of the batch builds to retrieve. Only batch builds that have this status will be retrieved.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bbfStatus :: Lens.Lens' BuildBatchFilter (Core.Maybe Types.StatusType)
bbfStatus = Lens.field @"status"
{-# DEPRECATED bbfStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON BuildBatchFilter where
  toJSON BuildBatchFilter {..} =
    Core.object (Core.catMaybes [("status" Core..=) Core.<$> status])
