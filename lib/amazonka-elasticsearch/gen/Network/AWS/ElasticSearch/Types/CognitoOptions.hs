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
    coIdentityPoolId,
    coEnabled,
    coUserPoolId,
    coRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Options to specify the Cognito user and identity pools for Kibana authentication. For more information, see <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-cognito-auth.html Amazon Cognito Authentication for Kibana> .
--
-- /See:/ 'mkCognitoOptions' smart constructor.
data CognitoOptions = CognitoOptions'
  { identityPoolId ::
      Lude.Maybe Lude.Text,
    enabled :: Lude.Maybe Lude.Bool,
    userPoolId :: Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CognitoOptions' with the minimum fields required to make a request.
--
-- * 'enabled' - Specifies the option to enable Cognito for Kibana authentication.
-- * 'identityPoolId' - Specifies the Cognito identity pool ID for Kibana authentication.
-- * 'roleARN' - Specifies the role ARN that provides Elasticsearch permissions for accessing Cognito resources.
-- * 'userPoolId' - Specifies the Cognito user pool ID for Kibana authentication.
mkCognitoOptions ::
  CognitoOptions
mkCognitoOptions =
  CognitoOptions'
    { identityPoolId = Lude.Nothing,
      enabled = Lude.Nothing,
      userPoolId = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | Specifies the Cognito identity pool ID for Kibana authentication.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coIdentityPoolId :: Lens.Lens' CognitoOptions (Lude.Maybe Lude.Text)
coIdentityPoolId = Lens.lens (identityPoolId :: CognitoOptions -> Lude.Maybe Lude.Text) (\s a -> s {identityPoolId = a} :: CognitoOptions)
{-# DEPRECATED coIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | Specifies the option to enable Cognito for Kibana authentication.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coEnabled :: Lens.Lens' CognitoOptions (Lude.Maybe Lude.Bool)
coEnabled = Lens.lens (enabled :: CognitoOptions -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: CognitoOptions)
{-# DEPRECATED coEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Specifies the Cognito user pool ID for Kibana authentication.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coUserPoolId :: Lens.Lens' CognitoOptions (Lude.Maybe Lude.Text)
coUserPoolId = Lens.lens (userPoolId :: CognitoOptions -> Lude.Maybe Lude.Text) (\s a -> s {userPoolId = a} :: CognitoOptions)
{-# DEPRECATED coUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | Specifies the role ARN that provides Elasticsearch permissions for accessing Cognito resources.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coRoleARN :: Lens.Lens' CognitoOptions (Lude.Maybe Lude.Text)
coRoleARN = Lens.lens (roleARN :: CognitoOptions -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: CognitoOptions)
{-# DEPRECATED coRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON CognitoOptions where
  parseJSON =
    Lude.withObject
      "CognitoOptions"
      ( \x ->
          CognitoOptions'
            Lude.<$> (x Lude..:? "IdentityPoolId")
            Lude.<*> (x Lude..:? "Enabled")
            Lude.<*> (x Lude..:? "UserPoolId")
            Lude.<*> (x Lude..:? "RoleArn")
      )

instance Lude.ToJSON CognitoOptions where
  toJSON CognitoOptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IdentityPoolId" Lude..=) Lude.<$> identityPoolId,
            ("Enabled" Lude..=) Lude.<$> enabled,
            ("UserPoolId" Lude..=) Lude.<$> userPoolId,
            ("RoleArn" Lude..=) Lude.<$> roleARN
          ]
      )
