{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.OidcMemberDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.OidcMemberDefinition
  ( OidcMemberDefinition (..),

    -- * Smart constructor
    mkOidcMemberDefinition,

    -- * Lenses
    omdGroups,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A list of user groups that exist in your OIDC Identity Provider (IdP). One to ten groups can be used to create a single private work team. When you add a user group to the list of @Groups@ , you can add that user group to one or more private work teams. If you add a user group to a private work team, all workers in that user group are added to the work team.
--
-- /See:/ 'mkOidcMemberDefinition' smart constructor.
newtype OidcMemberDefinition = OidcMemberDefinition'
  { groups ::
      Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OidcMemberDefinition' with the minimum fields required to make a request.
--
-- * 'groups' - A list of comma seperated strings that identifies user groups in your OIDC IdP. Each user group is made up of a group of private workers.
mkOidcMemberDefinition ::
  -- | 'groups'
  Lude.NonEmpty Lude.Text ->
  OidcMemberDefinition
mkOidcMemberDefinition pGroups_ =
  OidcMemberDefinition' {groups = pGroups_}

-- | A list of comma seperated strings that identifies user groups in your OIDC IdP. Each user group is made up of a group of private workers.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
omdGroups :: Lens.Lens' OidcMemberDefinition (Lude.NonEmpty Lude.Text)
omdGroups = Lens.lens (groups :: OidcMemberDefinition -> Lude.NonEmpty Lude.Text) (\s a -> s {groups = a} :: OidcMemberDefinition)
{-# DEPRECATED omdGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

instance Lude.FromJSON OidcMemberDefinition where
  parseJSON =
    Lude.withObject
      "OidcMemberDefinition"
      (\x -> OidcMemberDefinition' Lude.<$> (x Lude..: "Groups"))

instance Lude.ToJSON OidcMemberDefinition where
  toJSON OidcMemberDefinition' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Groups" Lude..= groups)])
