-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.RoleUsageType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.RoleUsageType
  ( RoleUsageType (..),

    -- * Smart constructor
    mkRoleUsageType,

    -- * Lenses
    rutResources,
    rutRegion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that contains details about how a service-linked role is used, if that information is returned by the service.
--
-- This data type is used as a response element in the 'GetServiceLinkedRoleDeletionStatus' operation.
--
-- /See:/ 'mkRoleUsageType' smart constructor.
data RoleUsageType = RoleUsageType'
  { resources ::
      Lude.Maybe [Lude.Text],
    region :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RoleUsageType' with the minimum fields required to make a request.
--
-- * 'region' - The name of the Region where the service-linked role is being used.
-- * 'resources' - The name of the resource that is using the service-linked role.
mkRoleUsageType ::
  RoleUsageType
mkRoleUsageType =
  RoleUsageType' {resources = Lude.Nothing, region = Lude.Nothing}

-- | The name of the resource that is using the service-linked role.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutResources :: Lens.Lens' RoleUsageType (Lude.Maybe [Lude.Text])
rutResources = Lens.lens (resources :: RoleUsageType -> Lude.Maybe [Lude.Text]) (\s a -> s {resources = a} :: RoleUsageType)
{-# DEPRECATED rutResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | The name of the Region where the service-linked role is being used.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rutRegion :: Lens.Lens' RoleUsageType (Lude.Maybe Lude.Text)
rutRegion = Lens.lens (region :: RoleUsageType -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: RoleUsageType)
{-# DEPRECATED rutRegion "Use generic-lens or generic-optics with 'region' instead." #-}

instance Lude.FromXML RoleUsageType where
  parseXML x =
    RoleUsageType'
      Lude.<$> ( x Lude..@? "Resources" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "Region")
