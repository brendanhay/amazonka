{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.CreateTestGridURL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a signed, short-term URL that can be passed to a Selenium @RemoteWebDriver@ constructor.
module Network.AWS.DeviceFarm.CreateTestGridURL
  ( -- * Creating a request
    CreateTestGridURL (..),
    mkCreateTestGridURL,

    -- ** Request lenses
    ctguProjectARN,
    ctguExpiresInSeconds,

    -- * Destructuring the response
    CreateTestGridURLResponse (..),
    mkCreateTestGridURLResponse,

    -- ** Response lenses
    ctgursExpires,
    ctgursUrl,
    ctgursResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateTestGridURL' smart constructor.
data CreateTestGridURL = CreateTestGridURL'
  { projectARN ::
      Lude.Text,
    expiresInSeconds :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTestGridURL' with the minimum fields required to make a request.
--
-- * 'expiresInSeconds' - Lifetime, in seconds, of the URL.
-- * 'projectARN' - ARN (from 'CreateTestGridProject' or 'ListTestGridProjects' ) to associate with the short-term URL.
mkCreateTestGridURL ::
  -- | 'projectARN'
  Lude.Text ->
  -- | 'expiresInSeconds'
  Lude.Natural ->
  CreateTestGridURL
mkCreateTestGridURL pProjectARN_ pExpiresInSeconds_ =
  CreateTestGridURL'
    { projectARN = pProjectARN_,
      expiresInSeconds = pExpiresInSeconds_
    }

-- | ARN (from 'CreateTestGridProject' or 'ListTestGridProjects' ) to associate with the short-term URL.
--
-- /Note:/ Consider using 'projectARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctguProjectARN :: Lens.Lens' CreateTestGridURL Lude.Text
ctguProjectARN = Lens.lens (projectARN :: CreateTestGridURL -> Lude.Text) (\s a -> s {projectARN = a} :: CreateTestGridURL)
{-# DEPRECATED ctguProjectARN "Use generic-lens or generic-optics with 'projectARN' instead." #-}

-- | Lifetime, in seconds, of the URL.
--
-- /Note:/ Consider using 'expiresInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctguExpiresInSeconds :: Lens.Lens' CreateTestGridURL Lude.Natural
ctguExpiresInSeconds = Lens.lens (expiresInSeconds :: CreateTestGridURL -> Lude.Natural) (\s a -> s {expiresInSeconds = a} :: CreateTestGridURL)
{-# DEPRECATED ctguExpiresInSeconds "Use generic-lens or generic-optics with 'expiresInSeconds' instead." #-}

instance Lude.AWSRequest CreateTestGridURL where
  type Rs CreateTestGridURL = CreateTestGridURLResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateTestGridURLResponse'
            Lude.<$> (x Lude..?> "expires")
            Lude.<*> (x Lude..?> "url")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTestGridURL where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.CreateTestGridUrl" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateTestGridURL where
  toJSON CreateTestGridURL' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("projectArn" Lude..= projectARN),
            Lude.Just ("expiresInSeconds" Lude..= expiresInSeconds)
          ]
      )

instance Lude.ToPath CreateTestGridURL where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTestGridURL where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateTestGridURLResponse' smart constructor.
data CreateTestGridURLResponse = CreateTestGridURLResponse'
  { expires ::
      Lude.Maybe Lude.Timestamp,
    url :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTestGridURLResponse' with the minimum fields required to make a request.
--
-- * 'expires' - The number of seconds the URL from 'CreateTestGridUrlResult$url' stays active.
-- * 'responseStatus' - The response status code.
-- * 'url' - A signed URL, expiring in 'CreateTestGridUrlRequest$expiresInSeconds' seconds, to be passed to a @RemoteWebDriver@ .
mkCreateTestGridURLResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTestGridURLResponse
mkCreateTestGridURLResponse pResponseStatus_ =
  CreateTestGridURLResponse'
    { expires = Lude.Nothing,
      url = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The number of seconds the URL from 'CreateTestGridUrlResult$url' stays active.
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgursExpires :: Lens.Lens' CreateTestGridURLResponse (Lude.Maybe Lude.Timestamp)
ctgursExpires = Lens.lens (expires :: CreateTestGridURLResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {expires = a} :: CreateTestGridURLResponse)
{-# DEPRECATED ctgursExpires "Use generic-lens or generic-optics with 'expires' instead." #-}

-- | A signed URL, expiring in 'CreateTestGridUrlRequest$expiresInSeconds' seconds, to be passed to a @RemoteWebDriver@ .
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgursUrl :: Lens.Lens' CreateTestGridURLResponse (Lude.Maybe Lude.Text)
ctgursUrl = Lens.lens (url :: CreateTestGridURLResponse -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: CreateTestGridURLResponse)
{-# DEPRECATED ctgursUrl "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctgursResponseStatus :: Lens.Lens' CreateTestGridURLResponse Lude.Int
ctgursResponseStatus = Lens.lens (responseStatus :: CreateTestGridURLResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTestGridURLResponse)
{-# DEPRECATED ctgursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
