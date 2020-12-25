{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MemberDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MemberDefinition
  ( MemberDefinition (..),

    -- * Smart constructor
    mkMemberDefinition,

    -- * Lenses
    mdCognitoMemberDefinition,
    mdOidcMemberDefinition,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.CognitoMemberDefinition as Types
import qualified Network.AWS.SageMaker.Types.OidcMemberDefinition as Types

-- | Defines an Amazon Cognito or your own OIDC IdP user group that is part of a work team.
--
-- /See:/ 'mkMemberDefinition' smart constructor.
data MemberDefinition = MemberDefinition'
  { -- | The Amazon Cognito user group that is part of the work team.
    cognitoMemberDefinition :: Core.Maybe Types.CognitoMemberDefinition,
    -- | A list user groups that exist in your OIDC Identity Provider (IdP). One to ten groups can be used to create a single private work team. When you add a user group to the list of @Groups@ , you can add that user group to one or more private work teams. If you add a user group to a private work team, all workers in that user group are added to the work team.
    oidcMemberDefinition :: Core.Maybe Types.OidcMemberDefinition
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MemberDefinition' value with any optional fields omitted.
mkMemberDefinition ::
  MemberDefinition
mkMemberDefinition =
  MemberDefinition'
    { cognitoMemberDefinition = Core.Nothing,
      oidcMemberDefinition = Core.Nothing
    }

-- | The Amazon Cognito user group that is part of the work team.
--
-- /Note:/ Consider using 'cognitoMemberDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdCognitoMemberDefinition :: Lens.Lens' MemberDefinition (Core.Maybe Types.CognitoMemberDefinition)
mdCognitoMemberDefinition = Lens.field @"cognitoMemberDefinition"
{-# DEPRECATED mdCognitoMemberDefinition "Use generic-lens or generic-optics with 'cognitoMemberDefinition' instead." #-}

-- | A list user groups that exist in your OIDC Identity Provider (IdP). One to ten groups can be used to create a single private work team. When you add a user group to the list of @Groups@ , you can add that user group to one or more private work teams. If you add a user group to a private work team, all workers in that user group are added to the work team.
--
-- /Note:/ Consider using 'oidcMemberDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdOidcMemberDefinition :: Lens.Lens' MemberDefinition (Core.Maybe Types.OidcMemberDefinition)
mdOidcMemberDefinition = Lens.field @"oidcMemberDefinition"
{-# DEPRECATED mdOidcMemberDefinition "Use generic-lens or generic-optics with 'oidcMemberDefinition' instead." #-}

instance Core.FromJSON MemberDefinition where
  toJSON MemberDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ ("CognitoMemberDefinition" Core..=)
              Core.<$> cognitoMemberDefinition,
            ("OidcMemberDefinition" Core..=) Core.<$> oidcMemberDefinition
          ]
      )

instance Core.FromJSON MemberDefinition where
  parseJSON =
    Core.withObject "MemberDefinition" Core.$
      \x ->
        MemberDefinition'
          Core.<$> (x Core..:? "CognitoMemberDefinition")
          Core.<*> (x Core..:? "OidcMemberDefinition")
