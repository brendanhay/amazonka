{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.GetIdentityPoolConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the configuration settings of an identity pool.
--
-- This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.
module Network.AWS.CognitoSync.GetIdentityPoolConfiguration
  ( -- * Creating a request
    GetIdentityPoolConfiguration (..),
    mkGetIdentityPoolConfiguration,

    -- ** Request lenses
    gipcIdentityPoolId,

    -- * Destructuring the response
    GetIdentityPoolConfigurationResponse (..),
    mkGetIdentityPoolConfigurationResponse,

    -- ** Response lenses
    gipcrsIdentityPoolId,
    gipcrsCognitoStreams,
    gipcrsPushSync,
    gipcrsResponseStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the GetIdentityPoolConfiguration operation.
--
-- /See:/ 'mkGetIdentityPoolConfiguration' smart constructor.
newtype GetIdentityPoolConfiguration = GetIdentityPoolConfiguration'
  { -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. This is the ID of the pool for which to return a configuration.
    identityPoolId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIdentityPoolConfiguration' with the minimum fields required to make a request.
--
-- * 'identityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. This is the ID of the pool for which to return a configuration.
mkGetIdentityPoolConfiguration ::
  -- | 'identityPoolId'
  Lude.Text ->
  GetIdentityPoolConfiguration
mkGetIdentityPoolConfiguration pIdentityPoolId_ =
  GetIdentityPoolConfiguration' {identityPoolId = pIdentityPoolId_}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. This is the ID of the pool for which to return a configuration.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipcIdentityPoolId :: Lens.Lens' GetIdentityPoolConfiguration Lude.Text
gipcIdentityPoolId = Lens.lens (identityPoolId :: GetIdentityPoolConfiguration -> Lude.Text) (\s a -> s {identityPoolId = a} :: GetIdentityPoolConfiguration)
{-# DEPRECATED gipcIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

instance Lude.AWSRequest GetIdentityPoolConfiguration where
  type
    Rs GetIdentityPoolConfiguration =
      GetIdentityPoolConfigurationResponse
  request = Req.get cognitoSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetIdentityPoolConfigurationResponse'
            Lude.<$> (x Lude..?> "IdentityPoolId")
            Lude.<*> (x Lude..?> "CognitoStreams")
            Lude.<*> (x Lude..?> "PushSync")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetIdentityPoolConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetIdentityPoolConfiguration where
  toPath GetIdentityPoolConfiguration' {..} =
    Lude.mconcat
      ["/identitypools/", Lude.toBS identityPoolId, "/configuration"]

instance Lude.ToQuery GetIdentityPoolConfiguration where
  toQuery = Lude.const Lude.mempty

-- | The output for the GetIdentityPoolConfiguration operation.
--
-- /See:/ 'mkGetIdentityPoolConfigurationResponse' smart constructor.
data GetIdentityPoolConfigurationResponse = GetIdentityPoolConfigurationResponse'
  { -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito.
    identityPoolId :: Lude.Maybe Lude.Text,
    -- | Options to apply to this identity pool for Amazon Cognito streams.
    cognitoStreams :: Lude.Maybe CognitoStreams,
    -- | Options to apply to this identity pool for push synchronization.
    pushSync :: Lude.Maybe PushSync,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIdentityPoolConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'identityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito.
-- * 'cognitoStreams' - Options to apply to this identity pool for Amazon Cognito streams.
-- * 'pushSync' - Options to apply to this identity pool for push synchronization.
-- * 'responseStatus' - The response status code.
mkGetIdentityPoolConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetIdentityPoolConfigurationResponse
mkGetIdentityPoolConfigurationResponse pResponseStatus_ =
  GetIdentityPoolConfigurationResponse'
    { identityPoolId =
        Lude.Nothing,
      cognitoStreams = Lude.Nothing,
      pushSync = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipcrsIdentityPoolId :: Lens.Lens' GetIdentityPoolConfigurationResponse (Lude.Maybe Lude.Text)
gipcrsIdentityPoolId = Lens.lens (identityPoolId :: GetIdentityPoolConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {identityPoolId = a} :: GetIdentityPoolConfigurationResponse)
{-# DEPRECATED gipcrsIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | Options to apply to this identity pool for Amazon Cognito streams.
--
-- /Note:/ Consider using 'cognitoStreams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipcrsCognitoStreams :: Lens.Lens' GetIdentityPoolConfigurationResponse (Lude.Maybe CognitoStreams)
gipcrsCognitoStreams = Lens.lens (cognitoStreams :: GetIdentityPoolConfigurationResponse -> Lude.Maybe CognitoStreams) (\s a -> s {cognitoStreams = a} :: GetIdentityPoolConfigurationResponse)
{-# DEPRECATED gipcrsCognitoStreams "Use generic-lens or generic-optics with 'cognitoStreams' instead." #-}

-- | Options to apply to this identity pool for push synchronization.
--
-- /Note:/ Consider using 'pushSync' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipcrsPushSync :: Lens.Lens' GetIdentityPoolConfigurationResponse (Lude.Maybe PushSync)
gipcrsPushSync = Lens.lens (pushSync :: GetIdentityPoolConfigurationResponse -> Lude.Maybe PushSync) (\s a -> s {pushSync = a} :: GetIdentityPoolConfigurationResponse)
{-# DEPRECATED gipcrsPushSync "Use generic-lens or generic-optics with 'pushSync' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipcrsResponseStatus :: Lens.Lens' GetIdentityPoolConfigurationResponse Lude.Int
gipcrsResponseStatus = Lens.lens (responseStatus :: GetIdentityPoolConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetIdentityPoolConfigurationResponse)
{-# DEPRECATED gipcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
