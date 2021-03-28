{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobExecutionStatusDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.JobExecutionStatusDetails
  ( JobExecutionStatusDetails (..)
  -- * Smart constructor
  , mkJobExecutionStatusDetails
  -- * Lenses
  , jesdDetailsMap
  ) where

import qualified Network.AWS.IoT.Types.DetailsKey as Types
import qualified Network.AWS.IoT.Types.DetailsValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details of the job execution status.
--
-- /See:/ 'mkJobExecutionStatusDetails' smart constructor.
newtype JobExecutionStatusDetails = JobExecutionStatusDetails'
  { detailsMap :: Core.Maybe (Core.HashMap Types.DetailsKey Types.DetailsValue)
    -- ^ The job execution status.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'JobExecutionStatusDetails' value with any optional fields omitted.
mkJobExecutionStatusDetails
    :: JobExecutionStatusDetails
mkJobExecutionStatusDetails
  = JobExecutionStatusDetails'{detailsMap = Core.Nothing}

-- | The job execution status.
--
-- /Note:/ Consider using 'detailsMap' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesdDetailsMap :: Lens.Lens' JobExecutionStatusDetails (Core.Maybe (Core.HashMap Types.DetailsKey Types.DetailsValue))
jesdDetailsMap = Lens.field @"detailsMap"
{-# INLINEABLE jesdDetailsMap #-}
{-# DEPRECATED detailsMap "Use generic-lens or generic-optics with 'detailsMap' instead"  #-}

instance Core.FromJSON JobExecutionStatusDetails where
        parseJSON
          = Core.withObject "JobExecutionStatusDetails" Core.$
              \ x ->
                JobExecutionStatusDetails' Core.<$> (x Core..:? "detailsMap")
