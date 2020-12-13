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
    ccClientId,
    ccUserPool,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Use this parameter to configure your Amazon Cognito workforce. A single Cognito workforce is created using and corresponds to a single <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito user pool> .
--
-- /See:/ 'mkCognitoConfig' smart constructor.
data CognitoConfig = CognitoConfig'
  { -- | The client ID for your Amazon Cognito user pool.
    clientId :: Lude.Text,
    -- | A <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html user pool> is a user directory in Amazon Cognito. With a user pool, your users can sign in to your web or mobile app through Amazon Cognito. Your users can also sign in through social identity providers like Google, Facebook, Amazon, or Apple, and through SAML identity providers.
    userPool :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CognitoConfig' with the minimum fields required to make a request.
--
-- * 'clientId' - The client ID for your Amazon Cognito user pool.
-- * 'userPool' - A <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html user pool> is a user directory in Amazon Cognito. With a user pool, your users can sign in to your web or mobile app through Amazon Cognito. Your users can also sign in through social identity providers like Google, Facebook, Amazon, or Apple, and through SAML identity providers.
mkCognitoConfig ::
  -- | 'clientId'
  Lude.Text ->
  -- | 'userPool'
  Lude.Text ->
  CognitoConfig
mkCognitoConfig pClientId_ pUserPool_ =
  CognitoConfig' {clientId = pClientId_, userPool = pUserPool_}

-- | The client ID for your Amazon Cognito user pool.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClientId :: Lens.Lens' CognitoConfig Lude.Text
ccClientId = Lens.lens (clientId :: CognitoConfig -> Lude.Text) (\s a -> s {clientId = a} :: CognitoConfig)
{-# DEPRECATED ccClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | A <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html user pool> is a user directory in Amazon Cognito. With a user pool, your users can sign in to your web or mobile app through Amazon Cognito. Your users can also sign in through social identity providers like Google, Facebook, Amazon, or Apple, and through SAML identity providers.
--
-- /Note:/ Consider using 'userPool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccUserPool :: Lens.Lens' CognitoConfig Lude.Text
ccUserPool = Lens.lens (userPool :: CognitoConfig -> Lude.Text) (\s a -> s {userPool = a} :: CognitoConfig)
{-# DEPRECATED ccUserPool "Use generic-lens or generic-optics with 'userPool' instead." #-}

instance Lude.FromJSON CognitoConfig where
  parseJSON =
    Lude.withObject
      "CognitoConfig"
      ( \x ->
          CognitoConfig'
            Lude.<$> (x Lude..: "ClientId") Lude.<*> (x Lude..: "UserPool")
      )

instance Lude.ToJSON CognitoConfig where
  toJSON CognitoConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ClientId" Lude..= clientId),
            Lude.Just ("UserPool" Lude..= userPool)
          ]
      )
