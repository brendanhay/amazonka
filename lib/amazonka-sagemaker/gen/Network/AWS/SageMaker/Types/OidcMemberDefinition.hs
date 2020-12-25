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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.Group as Types

-- | A list of user groups that exist in your OIDC Identity Provider (IdP). One to ten groups can be used to create a single private work team. When you add a user group to the list of @Groups@ , you can add that user group to one or more private work teams. If you add a user group to a private work team, all workers in that user group are added to the work team.
--
-- /See:/ 'mkOidcMemberDefinition' smart constructor.
newtype OidcMemberDefinition = OidcMemberDefinition'
  { -- | A list of comma seperated strings that identifies user groups in your OIDC IdP. Each user group is made up of a group of private workers.
    groups :: Core.NonEmpty Types.Group
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OidcMemberDefinition' value with any optional fields omitted.
mkOidcMemberDefinition ::
  -- | 'groups'
  Core.NonEmpty Types.Group ->
  OidcMemberDefinition
mkOidcMemberDefinition groups = OidcMemberDefinition' {groups}

-- | A list of comma seperated strings that identifies user groups in your OIDC IdP. Each user group is made up of a group of private workers.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
omdGroups :: Lens.Lens' OidcMemberDefinition (Core.NonEmpty Types.Group)
omdGroups = Lens.field @"groups"
{-# DEPRECATED omdGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

instance Core.FromJSON OidcMemberDefinition where
  toJSON OidcMemberDefinition {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Groups" Core..= groups)])

instance Core.FromJSON OidcMemberDefinition where
  parseJSON =
    Core.withObject "OidcMemberDefinition" Core.$
      \x -> OidcMemberDefinition' Core.<$> (x Core..: "Groups")
