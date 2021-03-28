{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.RoleUsageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.RoleUsageType
  ( RoleUsageType (..)
  -- * Smart constructor
  , mkRoleUsageType
  -- * Lenses
  , rutRegion
  , rutResources
  ) where

import qualified Network.AWS.IAM.Types.ArnType as Types
import qualified Network.AWS.IAM.Types.RegionNameType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that contains details about how a service-linked role is used, if that information is returned by the service.
--
-- This data type is used as a response element in the 'GetServiceLinkedRoleDeletionStatus' operation.
--
-- /See:/ 'mkRoleUsageType' smart constructor.
data RoleUsageType = RoleUsageType'
  { region :: Core.Maybe Types.RegionNameType
    -- ^ The name of the Region where the service-linked role is being used.
  , resources :: Core.Maybe [Types.ArnType]
    -- ^ The name of the resource that is using the service-linked role.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RoleUsageType' value with any optional fields omitted.
mkRoleUsageType
    :: RoleUsageType
mkRoleUsageType
  = RoleUsageType'{region = Core.Nothing, resources = Core.Nothing}

-- | The name of the Region where the service-linked role is being used.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutRegion :: Lens.Lens' RoleUsageType (Core.Maybe Types.RegionNameType)
rutRegion = Lens.field @"region"
{-# INLINEABLE rutRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

-- | The name of the resource that is using the service-linked role.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutResources :: Lens.Lens' RoleUsageType (Core.Maybe [Types.ArnType])
rutResources = Lens.field @"resources"
{-# INLINEABLE rutResources #-}
{-# DEPRECATED resources "Use generic-lens or generic-optics with 'resources' instead"  #-}

instance Core.FromXML RoleUsageType where
        parseXML x
          = RoleUsageType' Core.<$>
              (x Core..@? "Region") Core.<*>
                x Core..@? "Resources" Core..<@> Core.parseXMLList "member"
