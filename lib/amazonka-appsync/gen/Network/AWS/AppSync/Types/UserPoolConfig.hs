{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.UserPoolConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.UserPoolConfig
  ( UserPoolConfig (..),

    -- * Smart constructor
    mkUserPoolConfig,

    -- * Lenses
    upcUserPoolId,
    upcDefaultAction,
    upcAwsRegion,
    upcAppIdClientRegex,
  )
where

import Network.AWS.AppSync.Types.DefaultAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an Amazon Cognito user pool configuration.
--
-- /See:/ 'mkUserPoolConfig' smart constructor.
data UserPoolConfig = UserPoolConfig'
  { -- | The user pool ID.
    userPoolId :: Lude.Text,
    -- | The action that you want your GraphQL API to take when a request that uses Amazon Cognito user pool authentication doesn't match the Amazon Cognito user pool configuration.
    defaultAction :: DefaultAction,
    -- | The AWS Region in which the user pool was created.
    awsRegion :: Lude.Text,
    -- | A regular expression for validating the incoming Amazon Cognito user pool app client ID.
    appIdClientRegex :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserPoolConfig' with the minimum fields required to make a request.
--
-- * 'userPoolId' - The user pool ID.
-- * 'defaultAction' - The action that you want your GraphQL API to take when a request that uses Amazon Cognito user pool authentication doesn't match the Amazon Cognito user pool configuration.
-- * 'awsRegion' - The AWS Region in which the user pool was created.
-- * 'appIdClientRegex' - A regular expression for validating the incoming Amazon Cognito user pool app client ID.
mkUserPoolConfig ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'defaultAction'
  DefaultAction ->
  -- | 'awsRegion'
  Lude.Text ->
  UserPoolConfig
mkUserPoolConfig pUserPoolId_ pDefaultAction_ pAwsRegion_ =
  UserPoolConfig'
    { userPoolId = pUserPoolId_,
      defaultAction = pDefaultAction_,
      awsRegion = pAwsRegion_,
      appIdClientRegex = Lude.Nothing
    }

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcUserPoolId :: Lens.Lens' UserPoolConfig Lude.Text
upcUserPoolId = Lens.lens (userPoolId :: UserPoolConfig -> Lude.Text) (\s a -> s {userPoolId = a} :: UserPoolConfig)
{-# DEPRECATED upcUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The action that you want your GraphQL API to take when a request that uses Amazon Cognito user pool authentication doesn't match the Amazon Cognito user pool configuration.
--
-- /Note:/ Consider using 'defaultAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcDefaultAction :: Lens.Lens' UserPoolConfig DefaultAction
upcDefaultAction = Lens.lens (defaultAction :: UserPoolConfig -> DefaultAction) (\s a -> s {defaultAction = a} :: UserPoolConfig)
{-# DEPRECATED upcDefaultAction "Use generic-lens or generic-optics with 'defaultAction' instead." #-}

-- | The AWS Region in which the user pool was created.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcAwsRegion :: Lens.Lens' UserPoolConfig Lude.Text
upcAwsRegion = Lens.lens (awsRegion :: UserPoolConfig -> Lude.Text) (\s a -> s {awsRegion = a} :: UserPoolConfig)
{-# DEPRECATED upcAwsRegion "Use generic-lens or generic-optics with 'awsRegion' instead." #-}

-- | A regular expression for validating the incoming Amazon Cognito user pool app client ID.
--
-- /Note:/ Consider using 'appIdClientRegex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcAppIdClientRegex :: Lens.Lens' UserPoolConfig (Lude.Maybe Lude.Text)
upcAppIdClientRegex = Lens.lens (appIdClientRegex :: UserPoolConfig -> Lude.Maybe Lude.Text) (\s a -> s {appIdClientRegex = a} :: UserPoolConfig)
{-# DEPRECATED upcAppIdClientRegex "Use generic-lens or generic-optics with 'appIdClientRegex' instead." #-}

instance Lude.FromJSON UserPoolConfig where
  parseJSON =
    Lude.withObject
      "UserPoolConfig"
      ( \x ->
          UserPoolConfig'
            Lude.<$> (x Lude..: "userPoolId")
            Lude.<*> (x Lude..: "defaultAction")
            Lude.<*> (x Lude..: "awsRegion")
            Lude.<*> (x Lude..:? "appIdClientRegex")
      )

instance Lude.ToJSON UserPoolConfig where
  toJSON UserPoolConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("userPoolId" Lude..= userPoolId),
            Lude.Just ("defaultAction" Lude..= defaultAction),
            Lude.Just ("awsRegion" Lude..= awsRegion),
            ("appIdClientRegex" Lude..=) Lude.<$> appIdClientRegex
          ]
      )
