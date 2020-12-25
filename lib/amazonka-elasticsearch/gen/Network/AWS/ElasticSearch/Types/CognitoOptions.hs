{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.CognitoOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.CognitoOptions
  ( CognitoOptions (..),

    -- * Smart constructor
    mkCognitoOptions,

    -- * Lenses
    coEnabled,
    coIdentityPoolId,
    coRoleArn,
    coUserPoolId,
  )
where

import qualified Network.AWS.ElasticSearch.Types.IdentityPoolId as Types
import qualified Network.AWS.ElasticSearch.Types.RoleArn as Types
import qualified Network.AWS.ElasticSearch.Types.UserPoolId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Options to specify the Cognito user and identity pools for Kibana authentication. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
--
-- /See:/ 'mkCognitoOptions' smart constructor.
data CognitoOptions = CognitoOptions'
  { -- | Specifies the option to enable Cognito for Kibana authentication.
    enabled :: Core.Maybe Core.Bool,
    -- | Specifies the Cognito identity pool ID for Kibana authentication.
    identityPoolId :: Core.Maybe Types.IdentityPoolId,
    -- | Specifies the role ARN that provides Elasticsearch permissions for accessing Cognito resources.
    roleArn :: Core.Maybe Types.RoleArn,
    -- | Specifies the Cognito user pool ID for Kibana authentication.
    userPoolId :: Core.Maybe Types.UserPoolId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CognitoOptions' value with any optional fields omitted.
mkCognitoOptions ::
  CognitoOptions
mkCognitoOptions =
  CognitoOptions'
    { enabled = Core.Nothing,
      identityPoolId = Core.Nothing,
      roleArn = Core.Nothing,
      userPoolId = Core.Nothing
    }

-- | Specifies the option to enable Cognito for Kibana authentication.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coEnabled :: Lens.Lens' CognitoOptions (Core.Maybe Core.Bool)
coEnabled = Lens.field @"enabled"
{-# DEPRECATED coEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Specifies the Cognito identity pool ID for Kibana authentication.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coIdentityPoolId :: Lens.Lens' CognitoOptions (Core.Maybe Types.IdentityPoolId)
coIdentityPoolId = Lens.field @"identityPoolId"
{-# DEPRECATED coIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | Specifies the role ARN that provides Elasticsearch permissions for accessing Cognito resources.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coRoleArn :: Lens.Lens' CognitoOptions (Core.Maybe Types.RoleArn)
coRoleArn = Lens.field @"roleArn"
{-# DEPRECATED coRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | Specifies the Cognito user pool ID for Kibana authentication.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coUserPoolId :: Lens.Lens' CognitoOptions (Core.Maybe Types.UserPoolId)
coUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED coUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Core.FromJSON CognitoOptions where
  toJSON CognitoOptions {..} =
    Core.object
      ( Core.catMaybes
          [ ("Enabled" Core..=) Core.<$> enabled,
            ("IdentityPoolId" Core..=) Core.<$> identityPoolId,
            ("RoleArn" Core..=) Core.<$> roleArn,
            ("UserPoolId" Core..=) Core.<$> userPoolId
          ]
      )

instance Core.FromJSON CognitoOptions where
  parseJSON =
    Core.withObject "CognitoOptions" Core.$
      \x ->
        CognitoOptions'
          Core.<$> (x Core..:? "Enabled")
          Core.<*> (x Core..:? "IdentityPoolId")
          Core.<*> (x Core..:? "RoleArn")
          Core.<*> (x Core..:? "UserPoolId")
