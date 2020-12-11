{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.ListCuratedEnvironmentImages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about Docker images that are managed by AWS CodeBuild.
module Network.AWS.CodeBuild.ListCuratedEnvironmentImages
  ( -- * Creating a request
    ListCuratedEnvironmentImages (..),
    mkListCuratedEnvironmentImages,

    -- * Destructuring the response
    ListCuratedEnvironmentImagesResponse (..),
    mkListCuratedEnvironmentImagesResponse,

    -- ** Response lenses
    lceirsPlatforms,
    lceirsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListCuratedEnvironmentImages' smart constructor.
data ListCuratedEnvironmentImages = ListCuratedEnvironmentImages'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCuratedEnvironmentImages' with the minimum fields required to make a request.
mkListCuratedEnvironmentImages ::
  ListCuratedEnvironmentImages
mkListCuratedEnvironmentImages = ListCuratedEnvironmentImages'

instance Lude.AWSRequest ListCuratedEnvironmentImages where
  type
    Rs ListCuratedEnvironmentImages =
      ListCuratedEnvironmentImagesResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListCuratedEnvironmentImagesResponse'
            Lude.<$> (x Lude..?> "platforms" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListCuratedEnvironmentImages where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeBuild_20161006.ListCuratedEnvironmentImages" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListCuratedEnvironmentImages where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath ListCuratedEnvironmentImages where
  toPath = Lude.const "/"

instance Lude.ToQuery ListCuratedEnvironmentImages where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListCuratedEnvironmentImagesResponse' smart constructor.
data ListCuratedEnvironmentImagesResponse = ListCuratedEnvironmentImagesResponse'
  { platforms ::
      Lude.Maybe
        [EnvironmentPlatform],
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

-- | Creates a value of 'ListCuratedEnvironmentImagesResponse' with the minimum fields required to make a request.
--
-- * 'platforms' - Information about supported platforms for Docker images that are managed by AWS CodeBuild.
-- * 'responseStatus' - The response status code.
mkListCuratedEnvironmentImagesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListCuratedEnvironmentImagesResponse
mkListCuratedEnvironmentImagesResponse pResponseStatus_ =
  ListCuratedEnvironmentImagesResponse'
    { platforms = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about supported platforms for Docker images that are managed by AWS CodeBuild.
--
-- /Note:/ Consider using 'platforms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lceirsPlatforms :: Lens.Lens' ListCuratedEnvironmentImagesResponse (Lude.Maybe [EnvironmentPlatform])
lceirsPlatforms = Lens.lens (platforms :: ListCuratedEnvironmentImagesResponse -> Lude.Maybe [EnvironmentPlatform]) (\s a -> s {platforms = a} :: ListCuratedEnvironmentImagesResponse)
{-# DEPRECATED lceirsPlatforms "Use generic-lens or generic-optics with 'platforms' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lceirsResponseStatus :: Lens.Lens' ListCuratedEnvironmentImagesResponse Lude.Int
lceirsResponseStatus = Lens.lens (responseStatus :: ListCuratedEnvironmentImagesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListCuratedEnvironmentImagesResponse)
{-# DEPRECATED lceirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
