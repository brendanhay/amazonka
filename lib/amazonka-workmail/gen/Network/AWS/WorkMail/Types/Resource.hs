{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.Resource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkMail.Types.Resource
  ( Resource (..)
  -- * Smart constructor
  , mkResource
  -- * Lenses
  , rDisabledDate
  , rEmail
  , rEnabledDate
  , rId
  , rName
  , rState
  , rType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkMail.Types.EmailAddress as Types
import qualified Network.AWS.WorkMail.Types.EntityState as Types
import qualified Network.AWS.WorkMail.Types.ResourceName as Types
import qualified Network.AWS.WorkMail.Types.ResourceType as Types
import qualified Network.AWS.WorkMail.Types.WorkMailIdentifier as Types

-- | The representation of a resource.
--
-- /See:/ 'mkResource' smart constructor.
data Resource = Resource'
  { disabledDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date indicating when the resource was disabled from Amazon WorkMail use.
  , email :: Core.Maybe Types.EmailAddress
    -- ^ The email of the resource.
  , enabledDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date indicating when the resource was enabled for Amazon WorkMail use.
  , id :: Core.Maybe Types.WorkMailIdentifier
    -- ^ The identifier of the resource.
  , name :: Core.Maybe Types.ResourceName
    -- ^ The name of the resource.
  , state :: Core.Maybe Types.EntityState
    -- ^ The state of the resource, which can be ENABLED, DISABLED, or DELETED.
  , type' :: Core.Maybe Types.ResourceType
    -- ^ The type of the resource: equipment or room.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Resource' value with any optional fields omitted.
mkResource
    :: Resource
mkResource
  = Resource'{disabledDate = Core.Nothing, email = Core.Nothing,
              enabledDate = Core.Nothing, id = Core.Nothing, name = Core.Nothing,
              state = Core.Nothing, type' = Core.Nothing}

-- | The date indicating when the resource was disabled from Amazon WorkMail use.
--
-- /Note:/ Consider using 'disabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rDisabledDate :: Lens.Lens' Resource (Core.Maybe Core.NominalDiffTime)
rDisabledDate = Lens.field @"disabledDate"
{-# INLINEABLE rDisabledDate #-}
{-# DEPRECATED disabledDate "Use generic-lens or generic-optics with 'disabledDate' instead"  #-}

-- | The email of the resource.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEmail :: Lens.Lens' Resource (Core.Maybe Types.EmailAddress)
rEmail = Lens.field @"email"
{-# INLINEABLE rEmail #-}
{-# DEPRECATED email "Use generic-lens or generic-optics with 'email' instead"  #-}

-- | The date indicating when the resource was enabled for Amazon WorkMail use.
--
-- /Note:/ Consider using 'enabledDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rEnabledDate :: Lens.Lens' Resource (Core.Maybe Core.NominalDiffTime)
rEnabledDate = Lens.field @"enabledDate"
{-# INLINEABLE rEnabledDate #-}
{-# DEPRECATED enabledDate "Use generic-lens or generic-optics with 'enabledDate' instead"  #-}

-- | The identifier of the resource.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rId :: Lens.Lens' Resource (Core.Maybe Types.WorkMailIdentifier)
rId = Lens.field @"id"
{-# INLINEABLE rId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the resource.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rName :: Lens.Lens' Resource (Core.Maybe Types.ResourceName)
rName = Lens.field @"name"
{-# INLINEABLE rName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The state of the resource, which can be ENABLED, DISABLED, or DELETED.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rState :: Lens.Lens' Resource (Core.Maybe Types.EntityState)
rState = Lens.field @"state"
{-# INLINEABLE rState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The type of the resource: equipment or room.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rType :: Lens.Lens' Resource (Core.Maybe Types.ResourceType)
rType = Lens.field @"type'"
{-# INLINEABLE rType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON Resource where
        parseJSON
          = Core.withObject "Resource" Core.$
              \ x ->
                Resource' Core.<$>
                  (x Core..:? "DisabledDate") Core.<*> x Core..:? "Email" Core.<*>
                    x Core..:? "EnabledDate"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "State"
                    Core.<*> x Core..:? "Type"
