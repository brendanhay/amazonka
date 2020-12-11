{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListProvisioningTemplateVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A list of fleet provisioning template versions.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListProvisioningTemplateVersions
  ( -- * Creating a request
    ListProvisioningTemplateVersions (..),
    mkListProvisioningTemplateVersions,

    -- ** Request lenses
    lptvNextToken,
    lptvMaxResults,
    lptvTemplateName,

    -- * Destructuring the response
    ListProvisioningTemplateVersionsResponse (..),
    mkListProvisioningTemplateVersionsResponse,

    -- ** Response lenses
    lptvrsVersions,
    lptvrsNextToken,
    lptvrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListProvisioningTemplateVersions' smart constructor.
data ListProvisioningTemplateVersions = ListProvisioningTemplateVersions'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Natural,
    templateName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListProvisioningTemplateVersions' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to return at one time.
-- * 'nextToken' - A token to retrieve the next set of results.
-- * 'templateName' - The name of the fleet provisioning template.
mkListProvisioningTemplateVersions ::
  -- | 'templateName'
  Lude.Text ->
  ListProvisioningTemplateVersions
mkListProvisioningTemplateVersions pTemplateName_ =
  ListProvisioningTemplateVersions'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      templateName = pTemplateName_
    }

-- | A token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptvNextToken :: Lens.Lens' ListProvisioningTemplateVersions (Lude.Maybe Lude.Text)
lptvNextToken = Lens.lens (nextToken :: ListProvisioningTemplateVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListProvisioningTemplateVersions)
{-# DEPRECATED lptvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptvMaxResults :: Lens.Lens' ListProvisioningTemplateVersions (Lude.Maybe Lude.Natural)
lptvMaxResults = Lens.lens (maxResults :: ListProvisioningTemplateVersions -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListProvisioningTemplateVersions)
{-# DEPRECATED lptvMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The name of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptvTemplateName :: Lens.Lens' ListProvisioningTemplateVersions Lude.Text
lptvTemplateName = Lens.lens (templateName :: ListProvisioningTemplateVersions -> Lude.Text) (\s a -> s {templateName = a} :: ListProvisioningTemplateVersions)
{-# DEPRECATED lptvTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Page.AWSPager ListProvisioningTemplateVersions where
  page rq rs
    | Page.stop (rs Lens.^. lptvrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lptvrsVersions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lptvNextToken Lens..~ rs Lens.^. lptvrsNextToken

instance Lude.AWSRequest ListProvisioningTemplateVersions where
  type
    Rs ListProvisioningTemplateVersions =
      ListProvisioningTemplateVersionsResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListProvisioningTemplateVersionsResponse'
            Lude.<$> (x Lude..?> "versions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListProvisioningTemplateVersions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListProvisioningTemplateVersions where
  toPath ListProvisioningTemplateVersions' {..} =
    Lude.mconcat
      ["/provisioning-templates/", Lude.toBS templateName, "/versions"]

instance Lude.ToQuery ListProvisioningTemplateVersions where
  toQuery ListProvisioningTemplateVersions' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | /See:/ 'mkListProvisioningTemplateVersionsResponse' smart constructor.
data ListProvisioningTemplateVersionsResponse = ListProvisioningTemplateVersionsResponse'
  { versions ::
      Lude.Maybe
        [ProvisioningTemplateVersionSummary],
    nextToken ::
      Lude.Maybe
        Lude.Text,
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

-- | Creates a value of 'ListProvisioningTemplateVersionsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token to retrieve the next set of results.
-- * 'responseStatus' - The response status code.
-- * 'versions' - The list of fleet provisioning template versions.
mkListProvisioningTemplateVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListProvisioningTemplateVersionsResponse
mkListProvisioningTemplateVersionsResponse pResponseStatus_ =
  ListProvisioningTemplateVersionsResponse'
    { versions =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of fleet provisioning template versions.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptvrsVersions :: Lens.Lens' ListProvisioningTemplateVersionsResponse (Lude.Maybe [ProvisioningTemplateVersionSummary])
lptvrsVersions = Lens.lens (versions :: ListProvisioningTemplateVersionsResponse -> Lude.Maybe [ProvisioningTemplateVersionSummary]) (\s a -> s {versions = a} :: ListProvisioningTemplateVersionsResponse)
{-# DEPRECATED lptvrsVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | A token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptvrsNextToken :: Lens.Lens' ListProvisioningTemplateVersionsResponse (Lude.Maybe Lude.Text)
lptvrsNextToken = Lens.lens (nextToken :: ListProvisioningTemplateVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListProvisioningTemplateVersionsResponse)
{-# DEPRECATED lptvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lptvrsResponseStatus :: Lens.Lens' ListProvisioningTemplateVersionsResponse Lude.Int
lptvrsResponseStatus = Lens.lens (responseStatus :: ListProvisioningTemplateVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListProvisioningTemplateVersionsResponse)
{-# DEPRECATED lptvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
