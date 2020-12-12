{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.CognitoUserPoolConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.CognitoUserPoolConfig
  ( CognitoUserPoolConfig (..),

    -- * Smart constructor
    mkCognitoUserPoolConfig,

    -- * Lenses
    cupcAppIdClientRegex,
    cupcUserPoolId,
    cupcAwsRegion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an Amazon Cognito user pool configuration.
--
-- /See:/ 'mkCognitoUserPoolConfig' smart constructor.
data CognitoUserPoolConfig = CognitoUserPoolConfig'
  { appIdClientRegex ::
      Lude.Maybe Lude.Text,
    userPoolId :: Lude.Text,
    awsRegion :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CognitoUserPoolConfig' with the minimum fields required to make a request.
--
-- * 'appIdClientRegex' - A regular expression for validating the incoming Amazon Cognito user pool app client ID.
-- * 'awsRegion' - The AWS Region in which the user pool was created.
-- * 'userPoolId' - The user pool ID.
mkCognitoUserPoolConfig ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'awsRegion'
  Lude.Text ->
  CognitoUserPoolConfig
mkCognitoUserPoolConfig pUserPoolId_ pAwsRegion_ =
  CognitoUserPoolConfig'
    { appIdClientRegex = Lude.Nothing,
      userPoolId = pUserPoolId_,
      awsRegion = pAwsRegion_
    }

-- | A regular expression for validating the incoming Amazon Cognito user pool app client ID.
--
-- /Note:/ Consider using 'appIdClientRegex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcAppIdClientRegex :: Lens.Lens' CognitoUserPoolConfig (Lude.Maybe Lude.Text)
cupcAppIdClientRegex = Lens.lens (appIdClientRegex :: CognitoUserPoolConfig -> Lude.Maybe Lude.Text) (\s a -> s {appIdClientRegex = a} :: CognitoUserPoolConfig)
{-# DEPRECATED cupcAppIdClientRegex "Use generic-lens or generic-optics with 'appIdClientRegex' instead." #-}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcUserPoolId :: Lens.Lens' CognitoUserPoolConfig Lude.Text
cupcUserPoolId = Lens.lens (userPoolId :: CognitoUserPoolConfig -> Lude.Text) (\s a -> s {userPoolId = a} :: CognitoUserPoolConfig)
{-# DEPRECATED cupcUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The AWS Region in which the user pool was created.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcAwsRegion :: Lens.Lens' CognitoUserPoolConfig Lude.Text
cupcAwsRegion = Lens.lens (awsRegion :: CognitoUserPoolConfig -> Lude.Text) (\s a -> s {awsRegion = a} :: CognitoUserPoolConfig)
{-# DEPRECATED cupcAwsRegion "Use generic-lens or generic-optics with 'awsRegion' instead." #-}

instance Lude.FromJSON CognitoUserPoolConfig where
  parseJSON =
    Lude.withObject
      "CognitoUserPoolConfig"
      ( \x ->
          CognitoUserPoolConfig'
            Lude.<$> (x Lude..:? "appIdClientRegex")
            Lude.<*> (x Lude..: "userPoolId")
            Lude.<*> (x Lude..: "awsRegion")
      )

instance Lude.ToJSON CognitoUserPoolConfig where
  toJSON CognitoUserPoolConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("appIdClientRegex" Lude..=) Lude.<$> appIdClientRegex,
            Lude.Just ("userPoolId" Lude..= userPoolId),
            Lude.Just ("awsRegion" Lude..= awsRegion)
          ]
      )
