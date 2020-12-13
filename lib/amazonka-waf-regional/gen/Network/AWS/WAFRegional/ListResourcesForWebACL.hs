{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.ListResourcesForWebACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of resources associated with the specified web ACL.
module Network.AWS.WAFRegional.ListResourcesForWebACL
  ( -- * Creating a request
    ListResourcesForWebACL (..),
    mkListResourcesForWebACL,

    -- ** Request lenses
    lrfwaResourceType,
    lrfwaWebACLId,

    -- * Destructuring the response
    ListResourcesForWebACLResponse (..),
    mkListResourcesForWebACLResponse,

    -- ** Response lenses
    lrfwarsResourceARNs,
    lrfwarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WAFRegional.Types

-- | /See:/ 'mkListResourcesForWebACL' smart constructor.
data ListResourcesForWebACL = ListResourcesForWebACL'
  { -- | The type of resource to list, either an application load balancer or Amazon API Gateway.
    resourceType :: Lude.Maybe ResourceType,
    -- | The unique identifier (ID) of the web ACL for which to list the associated resources.
    webACLId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResourcesForWebACL' with the minimum fields required to make a request.
--
-- * 'resourceType' - The type of resource to list, either an application load balancer or Amazon API Gateway.
-- * 'webACLId' - The unique identifier (ID) of the web ACL for which to list the associated resources.
mkListResourcesForWebACL ::
  -- | 'webACLId'
  Lude.Text ->
  ListResourcesForWebACL
mkListResourcesForWebACL pWebACLId_ =
  ListResourcesForWebACL'
    { resourceType = Lude.Nothing,
      webACLId = pWebACLId_
    }

-- | The type of resource to list, either an application load balancer or Amazon API Gateway.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfwaResourceType :: Lens.Lens' ListResourcesForWebACL (Lude.Maybe ResourceType)
lrfwaResourceType = Lens.lens (resourceType :: ListResourcesForWebACL -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: ListResourcesForWebACL)
{-# DEPRECATED lrfwaResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The unique identifier (ID) of the web ACL for which to list the associated resources.
--
-- /Note:/ Consider using 'webACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfwaWebACLId :: Lens.Lens' ListResourcesForWebACL Lude.Text
lrfwaWebACLId = Lens.lens (webACLId :: ListResourcesForWebACL -> Lude.Text) (\s a -> s {webACLId = a} :: ListResourcesForWebACL)
{-# DEPRECATED lrfwaWebACLId "Use generic-lens or generic-optics with 'webACLId' instead." #-}

instance Lude.AWSRequest ListResourcesForWebACL where
  type Rs ListResourcesForWebACL = ListResourcesForWebACLResponse
  request = Req.postJSON wAFRegionalService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListResourcesForWebACLResponse'
            Lude.<$> (x Lude..?> "ResourceArns" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListResourcesForWebACL where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSWAF_Regional_20161128.ListResourcesForWebACL" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListResourcesForWebACL where
  toJSON ListResourcesForWebACL' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceType" Lude..=) Lude.<$> resourceType,
            Lude.Just ("WebACLId" Lude..= webACLId)
          ]
      )

instance Lude.ToPath ListResourcesForWebACL where
  toPath = Lude.const "/"

instance Lude.ToQuery ListResourcesForWebACL where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListResourcesForWebACLResponse' smart constructor.
data ListResourcesForWebACLResponse = ListResourcesForWebACLResponse'
  { -- | An array of ARNs (Amazon Resource Names) of the resources associated with the specified web ACL. An array with zero elements is returned if there are no resources associated with the web ACL.
    resourceARNs :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResourcesForWebACLResponse' with the minimum fields required to make a request.
--
-- * 'resourceARNs' - An array of ARNs (Amazon Resource Names) of the resources associated with the specified web ACL. An array with zero elements is returned if there are no resources associated with the web ACL.
-- * 'responseStatus' - The response status code.
mkListResourcesForWebACLResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListResourcesForWebACLResponse
mkListResourcesForWebACLResponse pResponseStatus_ =
  ListResourcesForWebACLResponse'
    { resourceARNs = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of ARNs (Amazon Resource Names) of the resources associated with the specified web ACL. An array with zero elements is returned if there are no resources associated with the web ACL.
--
-- /Note:/ Consider using 'resourceARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfwarsResourceARNs :: Lens.Lens' ListResourcesForWebACLResponse (Lude.Maybe [Lude.Text])
lrfwarsResourceARNs = Lens.lens (resourceARNs :: ListResourcesForWebACLResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {resourceARNs = a} :: ListResourcesForWebACLResponse)
{-# DEPRECATED lrfwarsResourceARNs "Use generic-lens or generic-optics with 'resourceARNs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfwarsResponseStatus :: Lens.Lens' ListResourcesForWebACLResponse Lude.Int
lrfwarsResponseStatus = Lens.lens (responseStatus :: ListResourcesForWebACLResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListResourcesForWebACLResponse)
{-# DEPRECATED lrfwarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
