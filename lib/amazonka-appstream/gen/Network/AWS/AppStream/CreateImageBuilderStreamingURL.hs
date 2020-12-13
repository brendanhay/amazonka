{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.CreateImageBuilderStreamingURL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a URL to start an image builder streaming session.
module Network.AWS.AppStream.CreateImageBuilderStreamingURL
  ( -- * Creating a request
    CreateImageBuilderStreamingURL (..),
    mkCreateImageBuilderStreamingURL,

    -- ** Request lenses
    cibsuName,
    cibsuValidity,

    -- * Destructuring the response
    CreateImageBuilderStreamingURLResponse (..),
    mkCreateImageBuilderStreamingURLResponse,

    -- ** Response lenses
    cibsursStreamingURL,
    cibsursExpires,
    cibsursResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateImageBuilderStreamingURL' smart constructor.
data CreateImageBuilderStreamingURL = CreateImageBuilderStreamingURL'
  { -- | The name of the image builder.
    name :: Lude.Text,
    -- | The time that the streaming URL will be valid, in seconds. Specify a value between 1 and 604800 seconds. The default is 3600 seconds.
    validity :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateImageBuilderStreamingURL' with the minimum fields required to make a request.
--
-- * 'name' - The name of the image builder.
-- * 'validity' - The time that the streaming URL will be valid, in seconds. Specify a value between 1 and 604800 seconds. The default is 3600 seconds.
mkCreateImageBuilderStreamingURL ::
  -- | 'name'
  Lude.Text ->
  CreateImageBuilderStreamingURL
mkCreateImageBuilderStreamingURL pName_ =
  CreateImageBuilderStreamingURL'
    { name = pName_,
      validity = Lude.Nothing
    }

-- | The name of the image builder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibsuName :: Lens.Lens' CreateImageBuilderStreamingURL Lude.Text
cibsuName = Lens.lens (name :: CreateImageBuilderStreamingURL -> Lude.Text) (\s a -> s {name = a} :: CreateImageBuilderStreamingURL)
{-# DEPRECATED cibsuName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The time that the streaming URL will be valid, in seconds. Specify a value between 1 and 604800 seconds. The default is 3600 seconds.
--
-- /Note:/ Consider using 'validity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibsuValidity :: Lens.Lens' CreateImageBuilderStreamingURL (Lude.Maybe Lude.Integer)
cibsuValidity = Lens.lens (validity :: CreateImageBuilderStreamingURL -> Lude.Maybe Lude.Integer) (\s a -> s {validity = a} :: CreateImageBuilderStreamingURL)
{-# DEPRECATED cibsuValidity "Use generic-lens or generic-optics with 'validity' instead." #-}

instance Lude.AWSRequest CreateImageBuilderStreamingURL where
  type
    Rs CreateImageBuilderStreamingURL =
      CreateImageBuilderStreamingURLResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateImageBuilderStreamingURLResponse'
            Lude.<$> (x Lude..?> "StreamingURL")
            Lude.<*> (x Lude..?> "Expires")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateImageBuilderStreamingURL where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "PhotonAdminProxyService.CreateImageBuilderStreamingURL" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateImageBuilderStreamingURL where
  toJSON CreateImageBuilderStreamingURL' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            ("Validity" Lude..=) Lude.<$> validity
          ]
      )

instance Lude.ToPath CreateImageBuilderStreamingURL where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateImageBuilderStreamingURL where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateImageBuilderStreamingURLResponse' smart constructor.
data CreateImageBuilderStreamingURLResponse = CreateImageBuilderStreamingURLResponse'
  { -- | The URL to start the AppStream 2.0 streaming session.
    streamingURL :: Lude.Maybe Lude.Text,
    -- | The elapsed time, in seconds after the Unix epoch, when this URL expires.
    expires :: Lude.Maybe Lude.Timestamp,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateImageBuilderStreamingURLResponse' with the minimum fields required to make a request.
--
-- * 'streamingURL' - The URL to start the AppStream 2.0 streaming session.
-- * 'expires' - The elapsed time, in seconds after the Unix epoch, when this URL expires.
-- * 'responseStatus' - The response status code.
mkCreateImageBuilderStreamingURLResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateImageBuilderStreamingURLResponse
mkCreateImageBuilderStreamingURLResponse pResponseStatus_ =
  CreateImageBuilderStreamingURLResponse'
    { streamingURL =
        Lude.Nothing,
      expires = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The URL to start the AppStream 2.0 streaming session.
--
-- /Note:/ Consider using 'streamingURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibsursStreamingURL :: Lens.Lens' CreateImageBuilderStreamingURLResponse (Lude.Maybe Lude.Text)
cibsursStreamingURL = Lens.lens (streamingURL :: CreateImageBuilderStreamingURLResponse -> Lude.Maybe Lude.Text) (\s a -> s {streamingURL = a} :: CreateImageBuilderStreamingURLResponse)
{-# DEPRECATED cibsursStreamingURL "Use generic-lens or generic-optics with 'streamingURL' instead." #-}

-- | The elapsed time, in seconds after the Unix epoch, when this URL expires.
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibsursExpires :: Lens.Lens' CreateImageBuilderStreamingURLResponse (Lude.Maybe Lude.Timestamp)
cibsursExpires = Lens.lens (expires :: CreateImageBuilderStreamingURLResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {expires = a} :: CreateImageBuilderStreamingURLResponse)
{-# DEPRECATED cibsursExpires "Use generic-lens or generic-optics with 'expires' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibsursResponseStatus :: Lens.Lens' CreateImageBuilderStreamingURLResponse Lude.Int
cibsursResponseStatus = Lens.lens (responseStatus :: CreateImageBuilderStreamingURLResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateImageBuilderStreamingURLResponse)
{-# DEPRECATED cibsursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
