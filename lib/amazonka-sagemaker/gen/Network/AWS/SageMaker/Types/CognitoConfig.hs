{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CognitoConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CognitoConfig
  ( CognitoConfig (..),

    -- * Smart constructor
    mkCognitoConfig,

    -- * Lenses
    ccUserPool,
    ccClientId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ClientId as Types
import qualified Network.AWS.SageMaker.Types.CognitoUserPool as Types

-- | Use this parameter to configure your Amazon Cognito workforce. A single Cognito workforce is created using and corresponds to a single <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool> .
--
-- /See:/ 'mkCognitoConfig' smart constructor.
data CognitoConfig = CognitoConfig'
  { -- | A <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html user pool> is a user directory in Amazon Cognito. With a user pool, your users can sign in to your web or mobile app through Amazon Cognito. Your users can also sign in through social identity providers like Google, Facebook, Amazon, or Apple, and through SAML identity providers.
    userPool :: Types.CognitoUserPool,
    -- | The client ID for your Amazon Cognito user pool.
    clientId :: Types.ClientId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CognitoConfig' value with any optional fields omitted.
mkCognitoConfig ::
  -- | 'userPool'
  Types.CognitoUserPool ->
  -- | 'clientId'
  Types.ClientId ->
  CognitoConfig
mkCognitoConfig userPool clientId =
  CognitoConfig' {userPool, clientId}

-- | A <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html user pool> is a user directory in Amazon Cognito. With a user pool, your users can sign in to your web or mobile app through Amazon Cognito. Your users can also sign in through social identity providers like Google, Facebook, Amazon, or Apple, and through SAML identity providers.
--
-- /Note:/ Consider using 'userPool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccUserPool :: Lens.Lens' CognitoConfig Types.CognitoUserPool
ccUserPool = Lens.field @"userPool"
{-# DEPRECATED ccUserPool "Use generic-lens or generic-optics with 'userPool' instead." #-}

-- | The client ID for your Amazon Cognito user pool.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClientId :: Lens.Lens' CognitoConfig Types.ClientId
ccClientId = Lens.field @"clientId"
{-# DEPRECATED ccClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

instance Core.FromJSON CognitoConfig where
  toJSON CognitoConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPool" Core..= userPool),
            Core.Just ("ClientId" Core..= clientId)
          ]
      )

instance Core.FromJSON CognitoConfig where
  parseJSON =
    Core.withObject "CognitoConfig" Core.$
      \x ->
        CognitoConfig'
          Core.<$> (x Core..: "UserPool") Core.<*> (x Core..: "ClientId")
