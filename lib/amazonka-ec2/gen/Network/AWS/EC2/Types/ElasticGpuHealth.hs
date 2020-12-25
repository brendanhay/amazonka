{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticGpuHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticGpuHealth
  ( ElasticGpuHealth (..),

    -- * Smart constructor
    mkElasticGpuHealth,

    -- * Lenses
    eghStatus,
  )
where

import qualified Network.AWS.EC2.Types.ElasticGpuStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the status of an Elastic Graphics accelerator.
--
-- /See:/ 'mkElasticGpuHealth' smart constructor.
newtype ElasticGpuHealth = ElasticGpuHealth'
  { -- | The health status.
    status :: Core.Maybe Types.ElasticGpuStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ElasticGpuHealth' value with any optional fields omitted.
mkElasticGpuHealth ::
  ElasticGpuHealth
mkElasticGpuHealth = ElasticGpuHealth' {status = Core.Nothing}

-- | The health status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eghStatus :: Lens.Lens' ElasticGpuHealth (Core.Maybe Types.ElasticGpuStatus)
eghStatus = Lens.field @"status"
{-# DEPRECATED eghStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromXML ElasticGpuHealth where
  parseXML x = ElasticGpuHealth' Core.<$> (x Core..@? "status")
