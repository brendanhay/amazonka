{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceSpecification
  ( InstanceSpecification (..),

    -- * Smart constructor
    mkInstanceSpecification,

    -- * Lenses
    isExcludeBootVolume,
    isInstanceId,
  )
where

import qualified Network.AWS.EC2.Types.InstanceId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The instance details to specify which volumes should be snapshotted.
--
-- /See:/ 'mkInstanceSpecification' smart constructor.
data InstanceSpecification = InstanceSpecification'
  { -- | Excludes the root volume from being snapshotted.
    excludeBootVolume :: Core.Maybe Core.Bool,
    -- | The instance to specify which volumes should be snapshotted.
    instanceId :: Core.Maybe Types.InstanceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceSpecification' value with any optional fields omitted.
mkInstanceSpecification ::
  InstanceSpecification
mkInstanceSpecification =
  InstanceSpecification'
    { excludeBootVolume = Core.Nothing,
      instanceId = Core.Nothing
    }

-- | Excludes the root volume from being snapshotted.
--
-- /Note:/ Consider using 'excludeBootVolume' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isExcludeBootVolume :: Lens.Lens' InstanceSpecification (Core.Maybe Core.Bool)
isExcludeBootVolume = Lens.field @"excludeBootVolume"
{-# DEPRECATED isExcludeBootVolume "Use generic-lens or generic-optics with 'excludeBootVolume' instead." #-}

-- | The instance to specify which volumes should be snapshotted.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isInstanceId :: Lens.Lens' InstanceSpecification (Core.Maybe Types.InstanceId)
isInstanceId = Lens.field @"instanceId"
{-# DEPRECATED isInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}
