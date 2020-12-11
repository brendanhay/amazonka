{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.ListTemplates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all the message templates that are associated with your Amazon Pinpoint account.
module Network.AWS.Pinpoint.ListTemplates
  ( -- * Creating a request
    ListTemplates (..),
    mkListTemplates,

    -- ** Request lenses
    ltTemplateType,
    ltPrefix,
    ltNextToken,
    ltPageSize,

    -- * Destructuring the response
    ListTemplatesResponse (..),
    mkListTemplatesResponse,

    -- ** Response lenses
    ltrsResponseStatus,
    ltrsTemplatesResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTemplates' smart constructor.
data ListTemplates = ListTemplates'
  { templateType ::
      Lude.Maybe Lude.Text,
    prefix :: Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    pageSize :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTemplates' with the minimum fields required to make a request.
--
-- * 'nextToken' - The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
-- * 'pageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
-- * 'prefix' - The substring to match in the names of the message templates to include in the results. If you specify this value, Amazon Pinpoint returns only those templates whose names begin with the value that you specify.
-- * 'templateType' - The type of message template to include in the results. Valid values are: EMAIL, PUSH, SMS, and VOICE. To include all types of templates in the results, don't include this parameter in your request.
mkListTemplates ::
  ListTemplates
mkListTemplates =
  ListTemplates'
    { templateType = Lude.Nothing,
      prefix = Lude.Nothing,
      nextToken = Lude.Nothing,
      pageSize = Lude.Nothing
    }

-- | The type of message template to include in the results. Valid values are: EMAIL, PUSH, SMS, and VOICE. To include all types of templates in the results, don't include this parameter in your request.
--
-- /Note:/ Consider using 'templateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltTemplateType :: Lens.Lens' ListTemplates (Lude.Maybe Lude.Text)
ltTemplateType = Lens.lens (templateType :: ListTemplates -> Lude.Maybe Lude.Text) (\s a -> s {templateType = a} :: ListTemplates)
{-# DEPRECATED ltTemplateType "Use generic-lens or generic-optics with 'templateType' instead." #-}

-- | The substring to match in the names of the message templates to include in the results. If you specify this value, Amazon Pinpoint returns only those templates whose names begin with the value that you specify.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltPrefix :: Lens.Lens' ListTemplates (Lude.Maybe Lude.Text)
ltPrefix = Lens.lens (prefix :: ListTemplates -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: ListTemplates)
{-# DEPRECATED ltPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltNextToken :: Lens.Lens' ListTemplates (Lude.Maybe Lude.Text)
ltNextToken = Lens.lens (nextToken :: ListTemplates -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTemplates)
{-# DEPRECATED ltNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltPageSize :: Lens.Lens' ListTemplates (Lude.Maybe Lude.Text)
ltPageSize = Lens.lens (pageSize :: ListTemplates -> Lude.Maybe Lude.Text) (\s a -> s {pageSize = a} :: ListTemplates)
{-# DEPRECATED ltPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Lude.AWSRequest ListTemplates where
  type Rs ListTemplates = ListTemplatesResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTemplatesResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (Lude.eitherParseJSON x)
      )

instance Lude.ToHeaders ListTemplates where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListTemplates where
  toPath = Lude.const "/v1/templates"

instance Lude.ToQuery ListTemplates where
  toQuery ListTemplates' {..} =
    Lude.mconcat
      [ "template-type" Lude.=: templateType,
        "prefix" Lude.=: prefix,
        "next-token" Lude.=: nextToken,
        "page-size" Lude.=: pageSize
      ]

-- | /See:/ 'mkListTemplatesResponse' smart constructor.
data ListTemplatesResponse = ListTemplatesResponse'
  { responseStatus ::
      Lude.Int,
    templatesResponse :: TemplatesResponse
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTemplatesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'templatesResponse' - Undocumented field.
mkListTemplatesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'templatesResponse'
  TemplatesResponse ->
  ListTemplatesResponse
mkListTemplatesResponse pResponseStatus_ pTemplatesResponse_ =
  ListTemplatesResponse'
    { responseStatus = pResponseStatus_,
      templatesResponse = pTemplatesResponse_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsResponseStatus :: Lens.Lens' ListTemplatesResponse Lude.Int
ltrsResponseStatus = Lens.lens (responseStatus :: ListTemplatesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTemplatesResponse)
{-# DEPRECATED ltrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'templatesResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrsTemplatesResponse :: Lens.Lens' ListTemplatesResponse TemplatesResponse
ltrsTemplatesResponse = Lens.lens (templatesResponse :: ListTemplatesResponse -> TemplatesResponse) (\s a -> s {templatesResponse = a} :: ListTemplatesResponse)
{-# DEPRECATED ltrsTemplatesResponse "Use generic-lens or generic-optics with 'templatesResponse' instead." #-}
