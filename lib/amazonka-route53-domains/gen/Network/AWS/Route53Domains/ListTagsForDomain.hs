{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.ListTagsForDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns all of the tags that are associated with the specified domain.
--
-- All tag operations are eventually consistent; subsequent operations might not immediately represent all issued operations.
module Network.AWS.Route53Domains.ListTagsForDomain
  ( -- * Creating a request
    ListTagsForDomain (..),
    mkListTagsForDomain,

    -- ** Request lenses
    ltfdDomainName,

    -- * Destructuring the response
    ListTagsForDomainResponse (..),
    mkListTagsForDomainResponse,

    -- ** Response lenses
    ltfdrsTagList,
    ltfdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | The ListTagsForDomainRequest includes the following elements.
--
-- /See:/ 'mkListTagsForDomain' smart constructor.
newtype ListTagsForDomain = ListTagsForDomain'
  { -- | The domain for which you want to get a list of tags.
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForDomain' with the minimum fields required to make a request.
--
-- * 'domainName' - The domain for which you want to get a list of tags.
mkListTagsForDomain ::
  -- | 'domainName'
  Lude.Text ->
  ListTagsForDomain
mkListTagsForDomain pDomainName_ =
  ListTagsForDomain' {domainName = pDomainName_}

-- | The domain for which you want to get a list of tags.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfdDomainName :: Lens.Lens' ListTagsForDomain Lude.Text
ltfdDomainName = Lens.lens (domainName :: ListTagsForDomain -> Lude.Text) (\s a -> s {domainName = a} :: ListTagsForDomain)
{-# DEPRECATED ltfdDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest ListTagsForDomain where
  type Rs ListTagsForDomain = ListTagsForDomainResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTagsForDomainResponse'
            Lude.<$> (x Lude..?> "TagList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTagsForDomain where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Route53Domains_v20140515.ListTagsForDomain" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListTagsForDomain where
  toJSON ListTagsForDomain' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("DomainName" Lude..= domainName)])

instance Lude.ToPath ListTagsForDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTagsForDomain where
  toQuery = Lude.const Lude.mempty

-- | The ListTagsForDomain response includes the following elements.
--
-- /See:/ 'mkListTagsForDomainResponse' smart constructor.
data ListTagsForDomainResponse = ListTagsForDomainResponse'
  { -- | A list of the tags that are associated with the specified domain.
    tagList :: [Tag],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTagsForDomainResponse' with the minimum fields required to make a request.
--
-- * 'tagList' - A list of the tags that are associated with the specified domain.
-- * 'responseStatus' - The response status code.
mkListTagsForDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTagsForDomainResponse
mkListTagsForDomainResponse pResponseStatus_ =
  ListTagsForDomainResponse'
    { tagList = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | A list of the tags that are associated with the specified domain.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfdrsTagList :: Lens.Lens' ListTagsForDomainResponse [Tag]
ltfdrsTagList = Lens.lens (tagList :: ListTagsForDomainResponse -> [Tag]) (\s a -> s {tagList = a} :: ListTagsForDomainResponse)
{-# DEPRECATED ltfdrsTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltfdrsResponseStatus :: Lens.Lens' ListTagsForDomainResponse Lude.Int
ltfdrsResponseStatus = Lens.lens (responseStatus :: ListTagsForDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTagsForDomainResponse)
{-# DEPRECATED ltfdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
