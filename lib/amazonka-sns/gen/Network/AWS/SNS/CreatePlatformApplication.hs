{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.CreatePlatformApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a platform application object for one of the supported push notification services, such as APNS and GCM (Firebase Cloud Messaging), to which devices and mobile apps may register. You must specify @PlatformPrincipal@ and @PlatformCredential@ attributes when using the @CreatePlatformApplication@ action.
--
-- @PlatformPrincipal@ and @PlatformCredential@ are received from the notification service.
--
--     * For @ADM@ , @PlatformPrincipal@ is @client id@ and @PlatformCredential@ is @client secret@ .
--
--
--     * For @Baidu@ , @PlatformPrincipal@ is @API key@ and @PlatformCredential@ is @secret key@ .
--
--
--     * For @APNS@ and @APNS_SANDBOX@ , @PlatformPrincipal@ is @SSL certificate@ and @PlatformCredential@ is @private key@ .
--
--
--     * For @GCM@ (Firebase Cloud Messaging), there is no @PlatformPrincipal@ and the @PlatformCredential@ is @API key@ .
--
--
--     * For @MPNS@ , @PlatformPrincipal@ is @TLS certificate@ and @PlatformCredential@ is @private key@ .
--
--
--     * For @WNS@ , @PlatformPrincipal@ is @Package Security Identifier@ and @PlatformCredential@ is @secret key@ .
--
--
-- You can use the returned @PlatformApplicationArn@ as an attribute for the @CreatePlatformEndpoint@ action.
module Network.AWS.SNS.CreatePlatformApplication
  ( -- * Creating a request
    CreatePlatformApplication (..),
    mkCreatePlatformApplication,

    -- ** Request lenses
    cpaName,
    cpaPlatform,
    cpaAttributes,

    -- * Destructuring the response
    CreatePlatformApplicationResponse (..),
    mkCreatePlatformApplicationResponse,

    -- ** Response lenses
    cparsPlatformApplicationARN,
    cparsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | Input for CreatePlatformApplication action.
--
-- /See:/ 'mkCreatePlatformApplication' smart constructor.
data CreatePlatformApplication = CreatePlatformApplication'
  { name ::
      Lude.Text,
    platform :: Lude.Text,
    attributes ::
      Lude.HashMap Lude.Text (Lude.Text)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePlatformApplication' with the minimum fields required to make a request.
--
-- * 'attributes' - For a list of attributes, see <https://docs.aws.amazon.com/sns/latest/api/API_SetPlatformApplicationAttributes.html SetPlatformApplicationAttributes>
-- * 'name' - Application names must be made up of only uppercase and lowercase ASCII letters, numbers, underscores, hyphens, and periods, and must be between 1 and 256 characters long.
-- * 'platform' - The following platforms are supported: ADM (Amazon Device Messaging), APNS (Apple Push Notification Service), APNS_SANDBOX, and GCM (Firebase Cloud Messaging).
mkCreatePlatformApplication ::
  -- | 'name'
  Lude.Text ->
  -- | 'platform'
  Lude.Text ->
  CreatePlatformApplication
mkCreatePlatformApplication pName_ pPlatform_ =
  CreatePlatformApplication'
    { name = pName_,
      platform = pPlatform_,
      attributes = Lude.mempty
    }

-- | Application names must be made up of only uppercase and lowercase ASCII letters, numbers, underscores, hyphens, and periods, and must be between 1 and 256 characters long.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpaName :: Lens.Lens' CreatePlatformApplication Lude.Text
cpaName = Lens.lens (name :: CreatePlatformApplication -> Lude.Text) (\s a -> s {name = a} :: CreatePlatformApplication)
{-# DEPRECATED cpaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The following platforms are supported: ADM (Amazon Device Messaging), APNS (Apple Push Notification Service), APNS_SANDBOX, and GCM (Firebase Cloud Messaging).
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpaPlatform :: Lens.Lens' CreatePlatformApplication Lude.Text
cpaPlatform = Lens.lens (platform :: CreatePlatformApplication -> Lude.Text) (\s a -> s {platform = a} :: CreatePlatformApplication)
{-# DEPRECATED cpaPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | For a list of attributes, see <https://docs.aws.amazon.com/sns/latest/api/API_SetPlatformApplicationAttributes.html SetPlatformApplicationAttributes>
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpaAttributes :: Lens.Lens' CreatePlatformApplication (Lude.HashMap Lude.Text (Lude.Text))
cpaAttributes = Lens.lens (attributes :: CreatePlatformApplication -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {attributes = a} :: CreatePlatformApplication)
{-# DEPRECATED cpaAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.AWSRequest CreatePlatformApplication where
  type
    Rs CreatePlatformApplication =
      CreatePlatformApplicationResponse
  request = Req.postQuery snsService
  response =
    Res.receiveXMLWrapper
      "CreatePlatformApplicationResult"
      ( \s h x ->
          CreatePlatformApplicationResponse'
            Lude.<$> (x Lude..@? "PlatformApplicationArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePlatformApplication where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreatePlatformApplication where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePlatformApplication where
  toQuery CreatePlatformApplication' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreatePlatformApplication" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "Name" Lude.=: name,
        "Platform" Lude.=: platform,
        "Attributes"
          Lude.=: Lude.toQueryMap "entry" "key" "value" attributes
      ]

-- | Response from CreatePlatformApplication action.
--
-- /See:/ 'mkCreatePlatformApplicationResponse' smart constructor.
data CreatePlatformApplicationResponse = CreatePlatformApplicationResponse'
  { platformApplicationARN ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePlatformApplicationResponse' with the minimum fields required to make a request.
--
-- * 'platformApplicationARN' - PlatformApplicationArn is returned.
-- * 'responseStatus' - The response status code.
mkCreatePlatformApplicationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePlatformApplicationResponse
mkCreatePlatformApplicationResponse pResponseStatus_ =
  CreatePlatformApplicationResponse'
    { platformApplicationARN =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | PlatformApplicationArn is returned.
--
-- /Note:/ Consider using 'platformApplicationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cparsPlatformApplicationARN :: Lens.Lens' CreatePlatformApplicationResponse (Lude.Maybe Lude.Text)
cparsPlatformApplicationARN = Lens.lens (platformApplicationARN :: CreatePlatformApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {platformApplicationARN = a} :: CreatePlatformApplicationResponse)
{-# DEPRECATED cparsPlatformApplicationARN "Use generic-lens or generic-optics with 'platformApplicationARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cparsResponseStatus :: Lens.Lens' CreatePlatformApplicationResponse Lude.Int
cparsResponseStatus = Lens.lens (responseStatus :: CreatePlatformApplicationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePlatformApplicationResponse)
{-# DEPRECATED cparsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
