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
    mdOidcMemberDefinition,
    mdCognitoMemberDefinition,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.CognitoMemberDefinition
import Network.AWS.SageMaker.Types.OidcMemberDefinition

-- | Defines an Amazon Cognito or your own OIDC IdP user group that is part of a work team.
--
-- /See:/ 'mkMemberDefinition' smart constructor.
data MemberDefinition = MemberDefinition'
  { oidcMemberDefinition ::
      Lude.Maybe OidcMemberDefinition,
    cognitoMemberDefinition ::
      Lude.Maybe CognitoMemberDefinition
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MemberDefinition' with the minimum fields required to make a request.
--
-- * 'cognitoMemberDefinition' - The Amazon Cognito user group that is part of the work team.
-- * 'oidcMemberDefinition' - A list user groups that exist in your OIDC Identity Provider (IdP). One to ten groups can be used to create a single private work team. When you add a user group to the list of @Groups@ , you can add that user group to one or more private work teams. If you add a user group to a private work team, all workers in that user group are added to the work team.
mkMemberDefinition ::
  MemberDefinition
mkMemberDefinition =
  MemberDefinition'
    { oidcMemberDefinition = Lude.Nothing,
      cognitoMemberDefinition = Lude.Nothing
    }

-- | A list user groups that exist in your OIDC Identity Provider (IdP). One to ten groups can be used to create a single private work team. When you add a user group to the list of @Groups@ , you can add that user group to one or more private work teams. If you add a user group to a private work team, all workers in that user group are added to the work team.
--
-- /Note:/ Consider using 'oidcMemberDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdOidcMemberDefinition :: Lens.Lens' MemberDefinition (Lude.Maybe OidcMemberDefinition)
mdOidcMemberDefinition = Lens.lens (oidcMemberDefinition :: MemberDefinition -> Lude.Maybe OidcMemberDefinition) (\s a -> s {oidcMemberDefinition = a} :: MemberDefinition)
{-# DEPRECATED mdOidcMemberDefinition "Use generic-lens or generic-optics with 'oidcMemberDefinition' instead." #-}

-- | The Amazon Cognito user group that is part of the work team.
--
-- /Note:/ Consider using 'cognitoMemberDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mdCognitoMemberDefinition :: Lens.Lens' MemberDefinition (Lude.Maybe CognitoMemberDefinition)
mdCognitoMemberDefinition = Lens.lens (cognitoMemberDefinition :: MemberDefinition -> Lude.Maybe CognitoMemberDefinition) (\s a -> s {cognitoMemberDefinition = a} :: MemberDefinition)
{-# DEPRECATED mdCognitoMemberDefinition "Use generic-lens or generic-optics with 'cognitoMemberDefinition' instead." #-}

instance Lude.FromJSON MemberDefinition where
  parseJSON =
    Lude.withObject
      "MemberDefinition"
      ( \x ->
          MemberDefinition'
            Lude.<$> (x Lude..:? "OidcMemberDefinition")
            Lude.<*> (x Lude..:? "CognitoMemberDefinition")
      )

instance Lude.ToJSON MemberDefinition where
  toJSON MemberDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("OidcMemberDefinition" Lude..=) Lude.<$> oidcMemberDefinition,
            ("CognitoMemberDefinition" Lude..=)
              Lude.<$> cognitoMemberDefinition
          ]
      )
