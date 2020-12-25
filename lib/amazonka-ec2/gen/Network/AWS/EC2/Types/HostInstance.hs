{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.HostInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HostInstance
  ( HostInstance (..),

    -- * Smart constructor
    mkHostInstance,

    -- * Lenses
    hiInstanceId,
    hiInstanceType,
    hiOwnerId,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an instance running on a Dedicated Host.
--
-- /See:/ 'mkHostInstance' smart constructor.
data HostInstance = HostInstance'
  { -- | The ID of instance that is running on the Dedicated Host.
    instanceId :: Core.Maybe Types.String,
    -- | The instance type (for example, @m3.medium@ ) of the running instance.
    instanceType :: Core.Maybe Types.String,
    -- | The ID of the AWS account that owns the instance.
    ownerId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HostInstance' value with any optional fields omitted.
mkHostInstance ::
  HostInstance
mkHostInstance =
  HostInstance'
    { instanceId = Core.Nothing,
      instanceType = Core.Nothing,
      ownerId = Core.Nothing
    }

-- | The ID of instance that is running on the Dedicated Host.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hiInstanceId :: Lens.Lens' HostInstance (Core.Maybe Types.String)
hiInstanceId = Lens.field @"instanceId"
{-# DEPRECATED hiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The instance type (for example, @m3.medium@ ) of the running instance.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hiInstanceType :: Lens.Lens' HostInstance (Core.Maybe Types.String)
hiInstanceType = Lens.field @"instanceType"
{-# DEPRECATED hiInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The ID of the AWS account that owns the instance.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hiOwnerId :: Lens.Lens' HostInstance (Core.Maybe Types.String)
hiOwnerId = Lens.field @"ownerId"
{-# DEPRECATED hiOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

instance Core.FromXML HostInstance where
  parseXML x =
    HostInstance'
      Core.<$> (x Core..@? "instanceId")
      Core.<*> (x Core..@? "instanceType")
      Core.<*> (x Core..@? "ownerId")
