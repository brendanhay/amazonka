{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.Principal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.Principal
  ( Principal (..),

    -- * Smart constructor
    mkPrincipal,

    -- * Lenses
    pRoles,
    pId,
    pType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkDocs.Types.PermissionInfo
import Network.AWS.WorkDocs.Types.PrincipalType

-- | Describes a resource.
--
-- /See:/ 'mkPrincipal' smart constructor.
data Principal = Principal'
  { -- | The permission information for the resource.
    roles :: Lude.Maybe [PermissionInfo],
    -- | The ID of the resource.
    id :: Lude.Maybe Lude.Text,
    -- | The type of resource.
    type' :: Lude.Maybe PrincipalType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Principal' with the minimum fields required to make a request.
--
-- * 'roles' - The permission information for the resource.
-- * 'id' - The ID of the resource.
-- * 'type'' - The type of resource.
mkPrincipal ::
  Principal
mkPrincipal =
  Principal'
    { roles = Lude.Nothing,
      id = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The permission information for the resource.
--
-- /Note:/ Consider using 'roles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pRoles :: Lens.Lens' Principal (Lude.Maybe [PermissionInfo])
pRoles = Lens.lens (roles :: Principal -> Lude.Maybe [PermissionInfo]) (\s a -> s {roles = a} :: Principal)
{-# DEPRECATED pRoles "Use generic-lens or generic-optics with 'roles' instead." #-}

-- | The ID of the resource.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pId :: Lens.Lens' Principal (Lude.Maybe Lude.Text)
pId = Lens.lens (id :: Principal -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Principal)
{-# DEPRECATED pId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of resource.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pType :: Lens.Lens' Principal (Lude.Maybe PrincipalType)
pType = Lens.lens (type' :: Principal -> Lude.Maybe PrincipalType) (\s a -> s {type' = a} :: Principal)
{-# DEPRECATED pType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON Principal where
  parseJSON =
    Lude.withObject
      "Principal"
      ( \x ->
          Principal'
            Lude.<$> (x Lude..:? "Roles" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Type")
      )
