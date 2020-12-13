{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.ListJobTemplates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a JSON array of up to twenty of your job templates. This will return the templates themselves, not just a list of them. To retrieve the next twenty templates, use the nextToken string returned with the array
--
-- This operation returns paginated results.
module Network.AWS.MediaConvert.ListJobTemplates
  ( -- * Creating a request
    ListJobTemplates (..),
    mkListJobTemplates,

    -- ** Request lenses
    ljtCategory,
    ljtListBy,
    ljtNextToken,
    ljtOrder,
    ljtMaxResults,

    -- * Destructuring the response
    ListJobTemplatesResponse (..),
    mkListJobTemplatesResponse,

    -- ** Response lenses
    ljtrsJobTemplates,
    ljtrsNextToken,
    ljtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListJobTemplates' smart constructor.
data ListJobTemplates = ListJobTemplates'
  { -- | Optionally, specify a job template category to limit responses to only job templates from that category.
    category :: Lude.Maybe Lude.Text,
    -- | Optional. When you request a list of job templates, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by name.
    listBy :: Lude.Maybe JobTemplateListBy,
    -- | Use this string, provided with the response to a previous request, to request the next batch of job templates.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Optional. When you request lists of resources, you can specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
    order :: Lude.Maybe Order,
    -- | Optional. Number of job templates, up to twenty, that will be returned at one time.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListJobTemplates' with the minimum fields required to make a request.
--
-- * 'category' - Optionally, specify a job template category to limit responses to only job templates from that category.
-- * 'listBy' - Optional. When you request a list of job templates, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by name.
-- * 'nextToken' - Use this string, provided with the response to a previous request, to request the next batch of job templates.
-- * 'order' - Optional. When you request lists of resources, you can specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
-- * 'maxResults' - Optional. Number of job templates, up to twenty, that will be returned at one time.
mkListJobTemplates ::
  ListJobTemplates
mkListJobTemplates =
  ListJobTemplates'
    { category = Lude.Nothing,
      listBy = Lude.Nothing,
      nextToken = Lude.Nothing,
      order = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Optionally, specify a job template category to limit responses to only job templates from that category.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljtCategory :: Lens.Lens' ListJobTemplates (Lude.Maybe Lude.Text)
ljtCategory = Lens.lens (category :: ListJobTemplates -> Lude.Maybe Lude.Text) (\s a -> s {category = a} :: ListJobTemplates)
{-# DEPRECATED ljtCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | Optional. When you request a list of job templates, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by name.
--
-- /Note:/ Consider using 'listBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljtListBy :: Lens.Lens' ListJobTemplates (Lude.Maybe JobTemplateListBy)
ljtListBy = Lens.lens (listBy :: ListJobTemplates -> Lude.Maybe JobTemplateListBy) (\s a -> s {listBy = a} :: ListJobTemplates)
{-# DEPRECATED ljtListBy "Use generic-lens or generic-optics with 'listBy' instead." #-}

-- | Use this string, provided with the response to a previous request, to request the next batch of job templates.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljtNextToken :: Lens.Lens' ListJobTemplates (Lude.Maybe Lude.Text)
ljtNextToken = Lens.lens (nextToken :: ListJobTemplates -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListJobTemplates)
{-# DEPRECATED ljtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Optional. When you request lists of resources, you can specify whether they are sorted in ASCENDING or DESCENDING order. Default varies by resource.
--
-- /Note:/ Consider using 'order' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljtOrder :: Lens.Lens' ListJobTemplates (Lude.Maybe Order)
ljtOrder = Lens.lens (order :: ListJobTemplates -> Lude.Maybe Order) (\s a -> s {order = a} :: ListJobTemplates)
{-# DEPRECATED ljtOrder "Use generic-lens or generic-optics with 'order' instead." #-}

-- | Optional. Number of job templates, up to twenty, that will be returned at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljtMaxResults :: Lens.Lens' ListJobTemplates (Lude.Maybe Lude.Natural)
ljtMaxResults = Lens.lens (maxResults :: ListJobTemplates -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListJobTemplates)
{-# DEPRECATED ljtMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListJobTemplates where
  page rq rs
    | Page.stop (rs Lens.^. ljtrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ljtrsJobTemplates) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ljtNextToken Lens..~ rs Lens.^. ljtrsNextToken

instance Lude.AWSRequest ListJobTemplates where
  type Rs ListJobTemplates = ListJobTemplatesResponse
  request = Req.get mediaConvertService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListJobTemplatesResponse'
            Lude.<$> (x Lude..?> "jobTemplates" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListJobTemplates where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListJobTemplates where
  toPath = Lude.const "/2017-08-29/jobTemplates"

instance Lude.ToQuery ListJobTemplates where
  toQuery ListJobTemplates' {..} =
    Lude.mconcat
      [ "category" Lude.=: category,
        "listBy" Lude.=: listBy,
        "nextToken" Lude.=: nextToken,
        "order" Lude.=: order,
        "maxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkListJobTemplatesResponse' smart constructor.
data ListJobTemplatesResponse = ListJobTemplatesResponse'
  { -- | List of Job templates.
    jobTemplates :: Lude.Maybe [JobTemplate],
    -- | Use this string to request the next batch of job templates.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListJobTemplatesResponse' with the minimum fields required to make a request.
--
-- * 'jobTemplates' - List of Job templates.
-- * 'nextToken' - Use this string to request the next batch of job templates.
-- * 'responseStatus' - The response status code.
mkListJobTemplatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListJobTemplatesResponse
mkListJobTemplatesResponse pResponseStatus_ =
  ListJobTemplatesResponse'
    { jobTemplates = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | List of Job templates.
--
-- /Note:/ Consider using 'jobTemplates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljtrsJobTemplates :: Lens.Lens' ListJobTemplatesResponse (Lude.Maybe [JobTemplate])
ljtrsJobTemplates = Lens.lens (jobTemplates :: ListJobTemplatesResponse -> Lude.Maybe [JobTemplate]) (\s a -> s {jobTemplates = a} :: ListJobTemplatesResponse)
{-# DEPRECATED ljtrsJobTemplates "Use generic-lens or generic-optics with 'jobTemplates' instead." #-}

-- | Use this string to request the next batch of job templates.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljtrsNextToken :: Lens.Lens' ListJobTemplatesResponse (Lude.Maybe Lude.Text)
ljtrsNextToken = Lens.lens (nextToken :: ListJobTemplatesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListJobTemplatesResponse)
{-# DEPRECATED ljtrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljtrsResponseStatus :: Lens.Lens' ListJobTemplatesResponse Lude.Int
ljtrsResponseStatus = Lens.lens (responseStatus :: ListJobTemplatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListJobTemplatesResponse)
{-# DEPRECATED ljtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
