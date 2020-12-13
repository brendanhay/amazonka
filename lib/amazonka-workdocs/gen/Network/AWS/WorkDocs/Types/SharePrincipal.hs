{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.SharePrincipal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.SharePrincipal
  ( SharePrincipal (..),

    -- * Smart constructor
    mkSharePrincipal,

    -- * Lenses
    spRole,
    spId,
    spType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkDocs.Types.PrincipalType
import Network.AWS.WorkDocs.Types.RoleType

-- | Describes the recipient type and ID, if available.
--
-- /See:/ 'mkSharePrincipal' smart constructor.
data SharePrincipal = SharePrincipal'
  { -- | The role of the recipient.
    role' :: RoleType,
    -- | The ID of the recipient.
    id :: Lude.Text,
    -- | The type of the recipient.
    type' :: PrincipalType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SharePrincipal' with the minimum fields required to make a request.
--
-- * 'role'' - The role of the recipient.
-- * 'id' - The ID of the recipient.
-- * 'type'' - The type of the recipient.
mkSharePrincipal ::
  -- | 'role''
  RoleType ->
  -- | 'id'
  Lude.Text ->
  -- | 'type''
  PrincipalType ->
  SharePrincipal
mkSharePrincipal pRole_ pId_ pType_ =
  SharePrincipal' {role' = pRole_, id = pId_, type' = pType_}

-- | The role of the recipient.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spRole :: Lens.Lens' SharePrincipal RoleType
spRole = Lens.lens (role' :: SharePrincipal -> RoleType) (\s a -> s {role' = a} :: SharePrincipal)
{-# DEPRECATED spRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The ID of the recipient.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spId :: Lens.Lens' SharePrincipal Lude.Text
spId = Lens.lens (id :: SharePrincipal -> Lude.Text) (\s a -> s {id = a} :: SharePrincipal)
{-# DEPRECATED spId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of the recipient.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spType :: Lens.Lens' SharePrincipal PrincipalType
spType = Lens.lens (type' :: SharePrincipal -> PrincipalType) (\s a -> s {type' = a} :: SharePrincipal)
{-# DEPRECATED spType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.ToJSON SharePrincipal where
  toJSON SharePrincipal' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Role" Lude..= role'),
            Lude.Just ("Id" Lude..= id),
            Lude.Just ("Type" Lude..= type')
          ]
      )
