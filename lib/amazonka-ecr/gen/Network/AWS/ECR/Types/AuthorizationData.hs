{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.AuthorizationData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.AuthorizationData
  ( AuthorizationData (..),

    -- * Smart constructor
    mkAuthorizationData,

    -- * Lenses
    adExpiresAt,
    adProxyEndpoint,
    adAuthorizationToken,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing authorization data for an Amazon ECR registry.
--
-- /See:/ 'mkAuthorizationData' smart constructor.
data AuthorizationData = AuthorizationData'
  { expiresAt ::
      Lude.Maybe Lude.Timestamp,
    proxyEndpoint :: Lude.Maybe Lude.Text,
    authorizationToken :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthorizationData' with the minimum fields required to make a request.
--
-- * 'authorizationToken' - A base64-encoded string that contains authorization data for the specified Amazon ECR registry. When the string is decoded, it is presented in the format @user:password@ for private registry authentication using @docker login@ .
-- * 'expiresAt' - The Unix time in seconds and milliseconds when the authorization token expires. Authorization tokens are valid for 12 hours.
-- * 'proxyEndpoint' - The registry URL to use for this authorization token in a @docker login@ command. The Amazon ECR registry URL format is @https://aws_account_id.dkr.ecr.region.amazonaws.com@ . For example, @https://012345678910.dkr.ecr.us-east-1.amazonaws.com@ ..
mkAuthorizationData ::
  AuthorizationData
mkAuthorizationData =
  AuthorizationData'
    { expiresAt = Lude.Nothing,
      proxyEndpoint = Lude.Nothing,
      authorizationToken = Lude.Nothing
    }

-- | The Unix time in seconds and milliseconds when the authorization token expires. Authorization tokens are valid for 12 hours.
--
-- /Note:/ Consider using 'expiresAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adExpiresAt :: Lens.Lens' AuthorizationData (Lude.Maybe Lude.Timestamp)
adExpiresAt = Lens.lens (expiresAt :: AuthorizationData -> Lude.Maybe Lude.Timestamp) (\s a -> s {expiresAt = a} :: AuthorizationData)
{-# DEPRECATED adExpiresAt "Use generic-lens or generic-optics with 'expiresAt' instead." #-}

-- | The registry URL to use for this authorization token in a @docker login@ command. The Amazon ECR registry URL format is @https://aws_account_id.dkr.ecr.region.amazonaws.com@ . For example, @https://012345678910.dkr.ecr.us-east-1.amazonaws.com@ ..
--
-- /Note:/ Consider using 'proxyEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adProxyEndpoint :: Lens.Lens' AuthorizationData (Lude.Maybe Lude.Text)
adProxyEndpoint = Lens.lens (proxyEndpoint :: AuthorizationData -> Lude.Maybe Lude.Text) (\s a -> s {proxyEndpoint = a} :: AuthorizationData)
{-# DEPRECATED adProxyEndpoint "Use generic-lens or generic-optics with 'proxyEndpoint' instead." #-}

-- | A base64-encoded string that contains authorization data for the specified Amazon ECR registry. When the string is decoded, it is presented in the format @user:password@ for private registry authentication using @docker login@ .
--
-- /Note:/ Consider using 'authorizationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adAuthorizationToken :: Lens.Lens' AuthorizationData (Lude.Maybe Lude.Text)
adAuthorizationToken = Lens.lens (authorizationToken :: AuthorizationData -> Lude.Maybe Lude.Text) (\s a -> s {authorizationToken = a} :: AuthorizationData)
{-# DEPRECATED adAuthorizationToken "Use generic-lens or generic-optics with 'authorizationToken' instead." #-}

instance Lude.FromJSON AuthorizationData where
  parseJSON =
    Lude.withObject
      "AuthorizationData"
      ( \x ->
          AuthorizationData'
            Lude.<$> (x Lude..:? "expiresAt")
            Lude.<*> (x Lude..:? "proxyEndpoint")
            Lude.<*> (x Lude..:? "authorizationToken")
      )
