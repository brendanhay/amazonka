{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.ListTemplateVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all the versions of a specific message template.
module Network.AWS.Pinpoint.ListTemplateVersions
  ( -- * Creating a request
    ListTemplateVersions (..),
    mkListTemplateVersions,

    -- ** Request lenses
    ltvTemplateName,
    ltvTemplateType,
    ltvNextToken,
    ltvPageSize,

    -- * Destructuring the response
    ListTemplateVersionsResponse (..),
    mkListTemplateVersionsResponse,

    -- ** Response lenses
    ltvrsTemplateVersionsResponse,
    ltvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListTemplateVersions' smart constructor.
data ListTemplateVersions = ListTemplateVersions'
  { -- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
    templateName :: Lude.Text,
    -- | The type of channel that the message template is designed for. Valid values are: EMAIL, PUSH, SMS, and VOICE.
    templateType :: Lude.Text,
    -- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
    pageSize :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTemplateVersions' with the minimum fields required to make a request.
--
-- * 'templateName' - The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
-- * 'templateType' - The type of channel that the message template is designed for. Valid values are: EMAIL, PUSH, SMS, and VOICE.
-- * 'nextToken' - The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
-- * 'pageSize' - The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
mkListTemplateVersions ::
  -- | 'templateName'
  Lude.Text ->
  -- | 'templateType'
  Lude.Text ->
  ListTemplateVersions
mkListTemplateVersions pTemplateName_ pTemplateType_ =
  ListTemplateVersions'
    { templateName = pTemplateName_,
      templateType = pTemplateType_,
      nextToken = Lude.Nothing,
      pageSize = Lude.Nothing
    }

-- | The name of the message template. A template name must start with an alphanumeric character and can contain a maximum of 128 characters. The characters can be alphanumeric characters, underscores (_), or hyphens (-). Template names are case sensitive.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvTemplateName :: Lens.Lens' ListTemplateVersions Lude.Text
ltvTemplateName = Lens.lens (templateName :: ListTemplateVersions -> Lude.Text) (\s a -> s {templateName = a} :: ListTemplateVersions)
{-# DEPRECATED ltvTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The type of channel that the message template is designed for. Valid values are: EMAIL, PUSH, SMS, and VOICE.
--
-- /Note:/ Consider using 'templateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvTemplateType :: Lens.Lens' ListTemplateVersions Lude.Text
ltvTemplateType = Lens.lens (templateType :: ListTemplateVersions -> Lude.Text) (\s a -> s {templateType = a} :: ListTemplateVersions)
{-# DEPRECATED ltvTemplateType "Use generic-lens or generic-optics with 'templateType' instead." #-}

-- | The  string that specifies which page of results to return in a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvNextToken :: Lens.Lens' ListTemplateVersions (Lude.Maybe Lude.Text)
ltvNextToken = Lens.lens (nextToken :: ListTemplateVersions -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListTemplateVersions)
{-# DEPRECATED ltvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvPageSize :: Lens.Lens' ListTemplateVersions (Lude.Maybe Lude.Text)
ltvPageSize = Lens.lens (pageSize :: ListTemplateVersions -> Lude.Maybe Lude.Text) (\s a -> s {pageSize = a} :: ListTemplateVersions)
{-# DEPRECATED ltvPageSize "Use generic-lens or generic-optics with 'pageSize' instead." #-}

instance Lude.AWSRequest ListTemplateVersions where
  type Rs ListTemplateVersions = ListTemplateVersionsResponse
  request = Req.get pinpointService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListTemplateVersionsResponse'
            Lude.<$> (Lude.eitherParseJSON x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListTemplateVersions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListTemplateVersions where
  toPath ListTemplateVersions' {..} =
    Lude.mconcat
      [ "/v1/templates/",
        Lude.toBS templateName,
        "/",
        Lude.toBS templateType,
        "/versions"
      ]

instance Lude.ToQuery ListTemplateVersions where
  toQuery ListTemplateVersions' {..} =
    Lude.mconcat
      ["next-token" Lude.=: nextToken, "page-size" Lude.=: pageSize]

-- | /See:/ 'mkListTemplateVersionsResponse' smart constructor.
data ListTemplateVersionsResponse = ListTemplateVersionsResponse'
  { templateVersionsResponse :: TemplateVersionsResponse,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListTemplateVersionsResponse' with the minimum fields required to make a request.
--
-- * 'templateVersionsResponse' -
-- * 'responseStatus' - The response status code.
mkListTemplateVersionsResponse ::
  -- | 'templateVersionsResponse'
  TemplateVersionsResponse ->
  -- | 'responseStatus'
  Lude.Int ->
  ListTemplateVersionsResponse
mkListTemplateVersionsResponse
  pTemplateVersionsResponse_
  pResponseStatus_ =
    ListTemplateVersionsResponse'
      { templateVersionsResponse =
          pTemplateVersionsResponse_,
        responseStatus = pResponseStatus_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'templateVersionsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvrsTemplateVersionsResponse :: Lens.Lens' ListTemplateVersionsResponse TemplateVersionsResponse
ltvrsTemplateVersionsResponse = Lens.lens (templateVersionsResponse :: ListTemplateVersionsResponse -> TemplateVersionsResponse) (\s a -> s {templateVersionsResponse = a} :: ListTemplateVersionsResponse)
{-# DEPRECATED ltvrsTemplateVersionsResponse "Use generic-lens or generic-optics with 'templateVersionsResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvrsResponseStatus :: Lens.Lens' ListTemplateVersionsResponse Lude.Int
ltvrsResponseStatus = Lens.lens (responseStatus :: ListTemplateVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListTemplateVersionsResponse)
{-# DEPRECATED ltvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
