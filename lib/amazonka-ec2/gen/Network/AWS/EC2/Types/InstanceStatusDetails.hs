{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceStatusDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceStatusDetails
  ( InstanceStatusDetails (..),

    -- * Smart constructor
    mkInstanceStatusDetails,

    -- * Lenses
    isdImpairedSince,
    isdName,
    isdStatus,
  )
where

import qualified Network.AWS.EC2.Types.StatusName as Types
import qualified Network.AWS.EC2.Types.StatusType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the instance status.
--
-- /See:/ 'mkInstanceStatusDetails' smart constructor.
data InstanceStatusDetails = InstanceStatusDetails'
  { -- | The time when a status check failed. For an instance that was launched and impaired, this is the time when the instance was launched.
    impairedSince :: Core.Maybe Core.UTCTime,
    -- | The type of instance status.
    name :: Core.Maybe Types.StatusName,
    -- | The status.
    status :: Core.Maybe Types.StatusType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'InstanceStatusDetails' value with any optional fields omitted.
mkInstanceStatusDetails ::
  InstanceStatusDetails
mkInstanceStatusDetails =
  InstanceStatusDetails'
    { impairedSince = Core.Nothing,
      name = Core.Nothing,
      status = Core.Nothing
    }

-- | The time when a status check failed. For an instance that was launched and impaired, this is the time when the instance was launched.
--
-- /Note:/ Consider using 'impairedSince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isdImpairedSince :: Lens.Lens' InstanceStatusDetails (Core.Maybe Core.UTCTime)
isdImpairedSince = Lens.field @"impairedSince"
{-# DEPRECATED isdImpairedSince "Use generic-lens or generic-optics with 'impairedSince' instead." #-}

-- | The type of instance status.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isdName :: Lens.Lens' InstanceStatusDetails (Core.Maybe Types.StatusName)
isdName = Lens.field @"name"
{-# DEPRECATED isdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isdStatus :: Lens.Lens' InstanceStatusDetails (Core.Maybe Types.StatusType)
isdStatus = Lens.field @"status"
{-# DEPRECATED isdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromXML InstanceStatusDetails where
  parseXML x =
    InstanceStatusDetails'
      Core.<$> (x Core..@? "impairedSince")
      Core.<*> (x Core..@? "name")
      Core.<*> (x Core..@? "status")
