{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.ListCompatibleImages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action returns a list of the different Amazon EC2 Amazon Machine Images (AMIs) that are owned by your AWS account that would be supported for use on a Snow device. Currently, supported AMIs are based on the CentOS 7 (x86_64) - with Updates HVM, Ubuntu Server 14.04 LTS (HVM), and Ubuntu 16.04 LTS - Xenial (HVM) images, available on the AWS Marketplace.
--
-- This operation returns paginated results.
module Network.AWS.Snowball.ListCompatibleImages
  ( -- * Creating a request
    ListCompatibleImages (..),
    mkListCompatibleImages,

    -- ** Request lenses
    lciNextToken,
    lciMaxResults,

    -- * Destructuring the response
    ListCompatibleImagesResponse (..),
    mkListCompatibleImagesResponse,

    -- ** Response lenses
    lcirsCompatibleImages,
    lcirsNextToken,
    lcirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Snowball.Types

-- | /See:/ 'mkListCompatibleImages' smart constructor.
data ListCompatibleImages = ListCompatibleImages'
  { -- | HTTP requests are stateless. To identify what object comes "next" in the list of compatible images, you can specify a value for @NextToken@ as the starting point for your list of returned images.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results for the list of compatible images. Currently, a Snowball Edge device can store 10 AMIs.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCompatibleImages' with the minimum fields required to make a request.
--
-- * 'nextToken' - HTTP requests are stateless. To identify what object comes "next" in the list of compatible images, you can specify a value for @NextToken@ as the starting point for your list of returned images.
-- * 'maxResults' - The maximum number of results for the list of compatible images. Currently, a Snowball Edge device can store 10 AMIs.
mkListCompatibleImages ::
  ListCompatibleImages
mkListCompatibleImages =
  ListCompatibleImages'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | HTTP requests are stateless. To identify what object comes "next" in the list of compatible images, you can specify a value for @NextToken@ as the starting point for your list of returned images.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciNextToken :: Lens.Lens' ListCompatibleImages (Lude.Maybe Lude.Text)
lciNextToken = Lens.lens (nextToken :: ListCompatibleImages -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCompatibleImages)
{-# DEPRECATED lciNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results for the list of compatible images. Currently, a Snowball Edge device can store 10 AMIs.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciMaxResults :: Lens.Lens' ListCompatibleImages (Lude.Maybe Lude.Natural)
lciMaxResults = Lens.lens (maxResults :: ListCompatibleImages -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListCompatibleImages)
{-# DEPRECATED lciMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListCompatibleImages where
  page rq rs
    | Page.stop (rs Lens.^. lcirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lcirsCompatibleImages) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lciNextToken Lens..~ rs Lens.^. lcirsNextToken

instance Lude.AWSRequest ListCompatibleImages where
  type Rs ListCompatibleImages = ListCompatibleImagesResponse
  request = Req.postJSON snowballService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListCompatibleImagesResponse'
            Lude.<$> (x Lude..?> "CompatibleImages" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListCompatibleImages where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSIESnowballJobManagementService.ListCompatibleImages" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListCompatibleImages where
  toJSON ListCompatibleImages' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListCompatibleImages where
  toPath = Lude.const "/"

instance Lude.ToQuery ListCompatibleImages where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListCompatibleImagesResponse' smart constructor.
data ListCompatibleImagesResponse = ListCompatibleImagesResponse'
  { -- | A JSON-formatted object that describes a compatible AMI, including the ID and name for a Snow device AMI.
    compatibleImages :: Lude.Maybe [CompatibleImage],
    -- | Because HTTP requests are stateless, this is the starting point for your next list of returned images.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListCompatibleImagesResponse' with the minimum fields required to make a request.
--
-- * 'compatibleImages' - A JSON-formatted object that describes a compatible AMI, including the ID and name for a Snow device AMI.
-- * 'nextToken' - Because HTTP requests are stateless, this is the starting point for your next list of returned images.
-- * 'responseStatus' - The response status code.
mkListCompatibleImagesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListCompatibleImagesResponse
mkListCompatibleImagesResponse pResponseStatus_ =
  ListCompatibleImagesResponse'
    { compatibleImages = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A JSON-formatted object that describes a compatible AMI, including the ID and name for a Snow device AMI.
--
-- /Note:/ Consider using 'compatibleImages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirsCompatibleImages :: Lens.Lens' ListCompatibleImagesResponse (Lude.Maybe [CompatibleImage])
lcirsCompatibleImages = Lens.lens (compatibleImages :: ListCompatibleImagesResponse -> Lude.Maybe [CompatibleImage]) (\s a -> s {compatibleImages = a} :: ListCompatibleImagesResponse)
{-# DEPRECATED lcirsCompatibleImages "Use generic-lens or generic-optics with 'compatibleImages' instead." #-}

-- | Because HTTP requests are stateless, this is the starting point for your next list of returned images.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirsNextToken :: Lens.Lens' ListCompatibleImagesResponse (Lude.Maybe Lude.Text)
lcirsNextToken = Lens.lens (nextToken :: ListCompatibleImagesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListCompatibleImagesResponse)
{-# DEPRECATED lcirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirsResponseStatus :: Lens.Lens' ListCompatibleImagesResponse Lude.Int
lcirsResponseStatus = Lens.lens (responseStatus :: ListCompatibleImagesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListCompatibleImagesResponse)
{-# DEPRECATED lcirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
