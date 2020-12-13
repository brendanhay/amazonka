{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.ListTemplates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the email templates present in your Amazon SES account in the current AWS Region.
--
-- You can execute this operation no more than once per second.
--
-- This operation returns paginated results.
module Network.AWS.SES.ListTemplates
  ( -- * Creating a request
    ListTemplates (..),
    mkListTemplates,

    -- ** Request lenses
    ltNextToken,
    ltMaxItems,

    -- * Destructuring the response
    ListTemplatesResponse (..),
    mkListTemplatesResponse,

    -- ** Response lenses
    ltrsTemplatesMetadata,
    ltrsNextToken,
    ltrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | /See:/ 'mkListTemplates' smart constructor.
data ListTemplates = ListTemplates'
  { -- | A token returned from a previous call to @ListTemplates@ to indicate the position in the list of email templates.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of templates to return. This value must be at least 1 and less than or equal to 10. If you do not specify a value, or if you specify a value less than 1 or greater than 10, the operation will return up to 10 results.
    maxItems :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTemplates' with the minimum fields required to make a request.
--
-- * 'nextToken' - A token returned from a previous call to @ListTemplates@ to indicate the position in the list of email templates.
-- * 'maxItems' - The maximum number of templates to return. This value must be at least 1 and less than or equal to 10. If you do not specify a value, or if you specify a value less than 1 or greater than 10, the operation will return up to 10 results.
mkListTemplates ::
  ListTemplates
mkListTemplates =
  ListTemplates' {nextToken = Lude.Nothing, maxItems = Lude.Nothing}

-- | A token returned from a previous call to @ListTemplates@ to indicate the position in the list of email templates.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTemplates (Lude.Maybe Lude.Text)
ltNextToken = Lens.lens (nextToken :: ListTemplates -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTemplates)
{-# DEPRECATED ltNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of templates to return. This value must be at least 1 and less than or equal to 10. If you do not specify a value, or if you specify a value less than 1 or greater than 10, the operation will return up to 10 results.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltMaxItems :: Lens.Lens' ListTemplates (Lude.Maybe Lude.Int)
ltMaxItems = Lens.lens (maxItems :: ListTemplates -> Lude.Maybe Lude.Int) (\s a -> s {maxItems = a} :: ListTemplates)
{-# DEPRECATED ltMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListTemplates where
  page rq rs
    | Page.stop (rs Lens.^. ltrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltrsTemplatesMetadata) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltNextToken Lens..~ rs Lens.^. ltrsNextToken

instance Lude.AWSRequest ListTemplates where
  type Rs ListTemplates = ListTemplatesResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "ListTemplatesResult"
      ( \s h x ->
          ListTemplatesResponse'
            Lude.<$> ( x Lude..@? "TemplatesMetadata" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTemplates where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListTemplates where
  toPath = Lude.const "/"

instance Lude.ToQuery ListTemplates where
  toQuery ListTemplates' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListTemplates" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "MaxItems" Lude.=: maxItems
      ]

-- | /See:/ 'mkListTemplatesResponse' smart constructor.
data ListTemplatesResponse = ListTemplatesResponse'
  { -- | An array the contains the name and creation time stamp for each template in your Amazon SES account.
    templatesMetadata :: Lude.Maybe [TemplateMetadata],
    -- | A token indicating that there are additional email templates available to be listed. Pass this token to a subsequent call to @ListTemplates@ to retrieve the next 50 email templates.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTemplatesResponse' with the minimum fields required to make a request.
--
-- * 'templatesMetadata' - An array the contains the name and creation time stamp for each template in your Amazon SES account.
-- * 'nextToken' - A token indicating that there are additional email templates available to be listed. Pass this token to a subsequent call to @ListTemplates@ to retrieve the next 50 email templates.
-- * 'responseStatus' - The response status code.
mkListTemplatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListTemplatesResponse
mkListTemplatesResponse pResponseStatus_ =
  ListTemplatesResponse'
    { templatesMetadata = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array the contains the name and creation time stamp for each template in your Amazon SES account.
--
-- /Note:/ Consider using 'templatesMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsTemplatesMetadata :: Lens.Lens' ListTemplatesResponse (Lude.Maybe [TemplateMetadata])
ltrsTemplatesMetadata = Lens.lens (templatesMetadata :: ListTemplatesResponse -> Lude.Maybe [TemplateMetadata]) (\s a -> s {templatesMetadata = a} :: ListTemplatesResponse)
{-# DEPRECATED ltrsTemplatesMetadata "Use generic-lens or generic-optics with 'templatesMetadata' instead." #-}

-- | A token indicating that there are additional email templates available to be listed. Pass this token to a subsequent call to @ListTemplates@ to retrieve the next 50 email templates.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsNextToken :: Lens.Lens' ListTemplatesResponse (Lude.Maybe Lude.Text)
ltrsNextToken = Lens.lens (nextToken :: ListTemplatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTemplatesResponse)
{-# DEPRECATED ltrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsResponseStatus :: Lens.Lens' ListTemplatesResponse Lude.Int
ltrsResponseStatus = Lens.lens (responseStatus :: ListTemplatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTemplatesResponse)
{-# DEPRECATED ltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
