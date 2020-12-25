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
    upcAwsRegion,
    upcDefaultAction,
    upcAppIdClientRegex,
  )
where

import qualified Network.AWS.AppSync.Types.DefaultAction as Types
import qualified Network.AWS.AppSync.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an Amazon Cognito user pool configuration.
--
-- /See:/ 'mkUserPoolConfig' smart constructor.
data UserPoolConfig = UserPoolConfig'
  { -- | The user pool ID.
    userPoolId :: Types.String,
    -- | The AWS Region in which the user pool was created.
    awsRegion :: Types.String,
    -- | The action that you want your GraphQL API to take when a request that uses Amazon Cognito user pool authentication doesn't match the Amazon Cognito user pool configuration.
    defaultAction :: Types.DefaultAction,
    -- | A regular expression for validating the incoming Amazon Cognito user pool app client ID.
    appIdClientRegex :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserPoolConfig' value with any optional fields omitted.
mkUserPoolConfig ::
  -- | 'userPoolId'
  Types.String ->
  -- | 'awsRegion'
  Types.String ->
  -- | 'defaultAction'
  Types.DefaultAction ->
  UserPoolConfig
mkUserPoolConfig userPoolId awsRegion defaultAction =
  UserPoolConfig'
    { userPoolId,
      awsRegion,
      defaultAction,
      appIdClientRegex = Core.Nothing
    }

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcUserPoolId :: Lens.Lens' UserPoolConfig Types.String
upcUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED upcUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The AWS Region in which the user pool was created.
--
-- /Note:/ Consider using 'awsRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcAwsRegion :: Lens.Lens' UserPoolConfig Types.String
upcAwsRegion = Lens.field @"awsRegion"
{-# DEPRECATED upcAwsRegion "Use generic-lens or generic-optics with 'awsRegion' instead." #-}

-- | The action that you want your GraphQL API to take when a request that uses Amazon Cognito user pool authentication doesn't match the Amazon Cognito user pool configuration.
--
-- /Note:/ Consider using 'defaultAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcDefaultAction :: Lens.Lens' UserPoolConfig Types.DefaultAction
upcDefaultAction = Lens.field @"defaultAction"
{-# DEPRECATED upcDefaultAction "Use generic-lens or generic-optics with 'defaultAction' instead." #-}

-- | A regular expression for validating the incoming Amazon Cognito user pool app client ID.
--
-- /Note:/ Consider using 'appIdClientRegex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcAppIdClientRegex :: Lens.Lens' UserPoolConfig (Core.Maybe Types.String)
upcAppIdClientRegex = Lens.field @"appIdClientRegex"
{-# DEPRECATED upcAppIdClientRegex "Use generic-lens or generic-optics with 'appIdClientRegex' instead." #-}

instance Core.FromJSON UserPoolConfig where
  toJSON UserPoolConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("userPoolId" Core..= userPoolId),
            Core.Just ("awsRegion" Core..= awsRegion),
            Core.Just ("defaultAction" Core..= defaultAction),
            ("appIdClientRegex" Core..=) Core.<$> appIdClientRegex
          ]
      )

instance Core.FromJSON UserPoolConfig where
  parseJSON =
    Core.withObject "UserPoolConfig" Core.$
      \x ->
        UserPoolConfig'
          Core.<$> (x Core..: "userPoolId")
          Core.<*> (x Core..: "awsRegion")
          Core.<*> (x Core..: "defaultAction")
          Core.<*> (x Core..:? "appIdClientRegex")
