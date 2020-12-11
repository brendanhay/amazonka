{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListProvisioningTemplates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the fleet provisioning templates in your AWS account.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListProvisioningTemplates
  ( -- * Creating a request
    ListProvisioningTemplates (..),
    mkListProvisioningTemplates,

    -- ** Request lenses
    lNextToken,
    lMaxResults,

    -- * Destructuring the response
    ListProvisioningTemplatesResponse (..),
    mkListProvisioningTemplatesResponse,

    -- ** Response lenses
    lrsTemplates,
    lrsNextToken,
    lrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListProvisioningTemplates' smart constructor.
data ListProvisioningTemplates = ListProvisioningTemplates'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListProvisioningTemplates' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to return at one time.
-- * 'nextToken' - A token to retrieve the next set of results.
mkListProvisioningTemplates ::
  ListProvisioningTemplates
mkListProvisioningTemplates =
  ListProvisioningTemplates'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lNextToken :: Lens.Lens' ListProvisioningTemplates (Lude.Maybe Lude.Text)
lNextToken = Lens.lens (nextToken :: ListProvisioningTemplates -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListProvisioningTemplates)
{-# DEPRECATED lNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lMaxResults :: Lens.Lens' ListProvisioningTemplates (Lude.Maybe Lude.Natural)
lMaxResults = Lens.lens (maxResults :: ListProvisioningTemplates -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListProvisioningTemplates)
{-# DEPRECATED lMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListProvisioningTemplates where
  page rq rs
    | Page.stop (rs Lens.^. lrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrsTemplates) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lNextToken Lens..~ rs Lens.^. lrsNextToken

instance Lude.AWSRequest ListProvisioningTemplates where
  type
    Rs ListProvisioningTemplates =
      ListProvisioningTemplatesResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListProvisioningTemplatesResponse'
            Lude.<$> (x Lude..?> "templates" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListProvisioningTemplates where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListProvisioningTemplates where
  toPath = Lude.const "/provisioning-templates"

instance Lude.ToQuery ListProvisioningTemplates where
  toQuery ListProvisioningTemplates' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListProvisioningTemplatesResponse' smart constructor.
data ListProvisioningTemplatesResponse = ListProvisioningTemplatesResponse'
  { templates ::
      Lude.Maybe
        [ProvisioningTemplateSummary],
    nextToken ::
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

-- | Creates a value of 'ListProvisioningTemplatesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token to retrieve the next set of results.
-- * 'responseStatus' - The response status code.
-- * 'templates' - A list of fleet provisioning templates
mkListProvisioningTemplatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListProvisioningTemplatesResponse
mkListProvisioningTemplatesResponse pResponseStatus_ =
  ListProvisioningTemplatesResponse'
    { templates = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of fleet provisioning templates
--
-- /Note:/ Consider using 'templates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsTemplates :: Lens.Lens' ListProvisioningTemplatesResponse (Lude.Maybe [ProvisioningTemplateSummary])
lrsTemplates = Lens.lens (templates :: ListProvisioningTemplatesResponse -> Lude.Maybe [ProvisioningTemplateSummary]) (\s a -> s {templates = a} :: ListProvisioningTemplatesResponse)
{-# DEPRECATED lrsTemplates "Use generic-lens or generic-optics with 'templates' instead." #-}

-- | A token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsNextToken :: Lens.Lens' ListProvisioningTemplatesResponse (Lude.Maybe Lude.Text)
lrsNextToken = Lens.lens (nextToken :: ListProvisioningTemplatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListProvisioningTemplatesResponse)
{-# DEPRECATED lrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrsResponseStatus :: Lens.Lens' ListProvisioningTemplatesResponse Lude.Int
lrsResponseStatus = Lens.lens (responseStatus :: ListProvisioningTemplatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListProvisioningTemplatesResponse)
{-# DEPRECATED lrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
