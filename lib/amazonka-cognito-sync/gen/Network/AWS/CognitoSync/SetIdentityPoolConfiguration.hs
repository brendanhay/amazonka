{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.SetIdentityPoolConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the necessary configuration for push sync.
--
-- This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.
module Network.AWS.CognitoSync.SetIdentityPoolConfiguration
  ( -- * Creating a request
    SetIdentityPoolConfiguration (..),
    mkSetIdentityPoolConfiguration,

    -- ** Request lenses
    sipcIdentityPoolId,
    sipcCognitoStreams,
    sipcPushSync,

    -- * Destructuring the response
    SetIdentityPoolConfigurationResponse (..),
    mkSetIdentityPoolConfigurationResponse,

    -- ** Response lenses
    sipcrsIdentityPoolId,
    sipcrsCognitoStreams,
    sipcrsPushSync,
    sipcrsResponseStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the SetIdentityPoolConfiguration operation.
--
-- /See:/ 'mkSetIdentityPoolConfiguration' smart constructor.
data SetIdentityPoolConfiguration = SetIdentityPoolConfiguration'
  { -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. This is the ID of the pool to modify.
    identityPoolId :: Lude.Text,
    -- | Options to apply to this identity pool for Amazon Cognito streams.
    cognitoStreams :: Lude.Maybe CognitoStreams,
    -- | Options to apply to this identity pool for push synchronization.
    pushSync :: Lude.Maybe PushSync
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetIdentityPoolConfiguration' with the minimum fields required to make a request.
--
-- * 'identityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. This is the ID of the pool to modify.
-- * 'cognitoStreams' - Options to apply to this identity pool for Amazon Cognito streams.
-- * 'pushSync' - Options to apply to this identity pool for push synchronization.
mkSetIdentityPoolConfiguration ::
  -- | 'identityPoolId'
  Lude.Text ->
  SetIdentityPoolConfiguration
mkSetIdentityPoolConfiguration pIdentityPoolId_ =
  SetIdentityPoolConfiguration'
    { identityPoolId = pIdentityPoolId_,
      cognitoStreams = Lude.Nothing,
      pushSync = Lude.Nothing
    }

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. This is the ID of the pool to modify.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipcIdentityPoolId :: Lens.Lens' SetIdentityPoolConfiguration Lude.Text
sipcIdentityPoolId = Lens.lens (identityPoolId :: SetIdentityPoolConfiguration -> Lude.Text) (\s a -> s {identityPoolId = a} :: SetIdentityPoolConfiguration)
{-# DEPRECATED sipcIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | Options to apply to this identity pool for Amazon Cognito streams.
--
-- /Note:/ Consider using 'cognitoStreams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipcCognitoStreams :: Lens.Lens' SetIdentityPoolConfiguration (Lude.Maybe CognitoStreams)
sipcCognitoStreams = Lens.lens (cognitoStreams :: SetIdentityPoolConfiguration -> Lude.Maybe CognitoStreams) (\s a -> s {cognitoStreams = a} :: SetIdentityPoolConfiguration)
{-# DEPRECATED sipcCognitoStreams "Use generic-lens or generic-optics with 'cognitoStreams' instead." #-}

-- | Options to apply to this identity pool for push synchronization.
--
-- /Note:/ Consider using 'pushSync' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipcPushSync :: Lens.Lens' SetIdentityPoolConfiguration (Lude.Maybe PushSync)
sipcPushSync = Lens.lens (pushSync :: SetIdentityPoolConfiguration -> Lude.Maybe PushSync) (\s a -> s {pushSync = a} :: SetIdentityPoolConfiguration)
{-# DEPRECATED sipcPushSync "Use generic-lens or generic-optics with 'pushSync' instead." #-}

instance Lude.AWSRequest SetIdentityPoolConfiguration where
  type
    Rs SetIdentityPoolConfiguration =
      SetIdentityPoolConfigurationResponse
  request = Req.postJSON cognitoSyncService
  response =
    Res.receiveJSON
      ( \s h x ->
          SetIdentityPoolConfigurationResponse'
            Lude.<$> (x Lude..?> "IdentityPoolId")
            Lude.<*> (x Lude..?> "CognitoStreams")
            Lude.<*> (x Lude..?> "PushSync")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetIdentityPoolConfiguration where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SetIdentityPoolConfiguration where
  toJSON SetIdentityPoolConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CognitoStreams" Lude..=) Lude.<$> cognitoStreams,
            ("PushSync" Lude..=) Lude.<$> pushSync
          ]
      )

instance Lude.ToPath SetIdentityPoolConfiguration where
  toPath SetIdentityPoolConfiguration' {..} =
    Lude.mconcat
      ["/identitypools/", Lude.toBS identityPoolId, "/configuration"]

instance Lude.ToQuery SetIdentityPoolConfiguration where
  toQuery = Lude.const Lude.mempty

-- | The output for the SetIdentityPoolConfiguration operation
--
-- /See:/ 'mkSetIdentityPoolConfigurationResponse' smart constructor.
data SetIdentityPoolConfigurationResponse = SetIdentityPoolConfigurationResponse'
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

-- | Creates a value of 'SetIdentityPoolConfigurationResponse' with the minimum fields required to make a request.
--
-- * 'identityPoolId' - A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito.
-- * 'cognitoStreams' - Options to apply to this identity pool for Amazon Cognito streams.
-- * 'pushSync' - Options to apply to this identity pool for push synchronization.
-- * 'responseStatus' - The response status code.
mkSetIdentityPoolConfigurationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetIdentityPoolConfigurationResponse
mkSetIdentityPoolConfigurationResponse pResponseStatus_ =
  SetIdentityPoolConfigurationResponse'
    { identityPoolId =
        Lude.Nothing,
      cognitoStreams = Lude.Nothing,
      pushSync = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipcrsIdentityPoolId :: Lens.Lens' SetIdentityPoolConfigurationResponse (Lude.Maybe Lude.Text)
sipcrsIdentityPoolId = Lens.lens (identityPoolId :: SetIdentityPoolConfigurationResponse -> Lude.Maybe Lude.Text) (\s a -> s {identityPoolId = a} :: SetIdentityPoolConfigurationResponse)
{-# DEPRECATED sipcrsIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | Options to apply to this identity pool for Amazon Cognito streams.
--
-- /Note:/ Consider using 'cognitoStreams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipcrsCognitoStreams :: Lens.Lens' SetIdentityPoolConfigurationResponse (Lude.Maybe CognitoStreams)
sipcrsCognitoStreams = Lens.lens (cognitoStreams :: SetIdentityPoolConfigurationResponse -> Lude.Maybe CognitoStreams) (\s a -> s {cognitoStreams = a} :: SetIdentityPoolConfigurationResponse)
{-# DEPRECATED sipcrsCognitoStreams "Use generic-lens or generic-optics with 'cognitoStreams' instead." #-}

-- | Options to apply to this identity pool for push synchronization.
--
-- /Note:/ Consider using 'pushSync' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipcrsPushSync :: Lens.Lens' SetIdentityPoolConfigurationResponse (Lude.Maybe PushSync)
sipcrsPushSync = Lens.lens (pushSync :: SetIdentityPoolConfigurationResponse -> Lude.Maybe PushSync) (\s a -> s {pushSync = a} :: SetIdentityPoolConfigurationResponse)
{-# DEPRECATED sipcrsPushSync "Use generic-lens or generic-optics with 'pushSync' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sipcrsResponseStatus :: Lens.Lens' SetIdentityPoolConfigurationResponse Lude.Int
sipcrsResponseStatus = Lens.lens (responseStatus :: SetIdentityPoolConfigurationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetIdentityPoolConfigurationResponse)
{-# DEPRECATED sipcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
