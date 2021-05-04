{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.ListTemplateVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all the versions of a specific message
-- template.
module Network.AWS.Pinpoint.ListTemplateVersions
  ( -- * Creating a Request
    ListTemplateVersions (..),
    newListTemplateVersions,

    -- * Request Lenses
    listTemplateVersions_nextToken,
    listTemplateVersions_pageSize,
    listTemplateVersions_templateName,
    listTemplateVersions_templateType,

    -- * Destructuring the Response
    ListTemplateVersionsResponse (..),
    newListTemplateVersionsResponse,

    -- * Response Lenses
    listTemplateVersionsResponse_httpStatus,
    listTemplateVersionsResponse_templateVersionsResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTemplateVersions' smart constructor.
data ListTemplateVersions = ListTemplateVersions'
  { -- | The string that specifies which page of results to return in a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to include in each page of a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    pageSize :: Prelude.Maybe Prelude.Text,
    -- | The name of the message template. A template name must start with an
    -- alphanumeric character and can contain a maximum of 128 characters. The
    -- characters can be alphanumeric characters, underscores (_), or hyphens
    -- (-). Template names are case sensitive.
    templateName :: Prelude.Text,
    -- | The type of channel that the message template is designed for. Valid
    -- values are: EMAIL, PUSH, SMS, and VOICE.
    templateType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListTemplateVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTemplateVersions_nextToken' - The string that specifies which page of results to return in a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'pageSize', 'listTemplateVersions_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'templateName', 'listTemplateVersions_templateName' - The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
--
-- 'templateType', 'listTemplateVersions_templateType' - The type of channel that the message template is designed for. Valid
-- values are: EMAIL, PUSH, SMS, and VOICE.
newListTemplateVersions ::
  -- | 'templateName'
  Prelude.Text ->
  -- | 'templateType'
  Prelude.Text ->
  ListTemplateVersions
newListTemplateVersions pTemplateName_ pTemplateType_ =
  ListTemplateVersions'
    { nextToken = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      templateName = pTemplateName_,
      templateType = pTemplateType_
    }

-- | The string that specifies which page of results to return in a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
listTemplateVersions_nextToken :: Lens.Lens' ListTemplateVersions (Prelude.Maybe Prelude.Text)
listTemplateVersions_nextToken = Lens.lens (\ListTemplateVersions' {nextToken} -> nextToken) (\s@ListTemplateVersions' {} a -> s {nextToken = a} :: ListTemplateVersions)

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
listTemplateVersions_pageSize :: Lens.Lens' ListTemplateVersions (Prelude.Maybe Prelude.Text)
listTemplateVersions_pageSize = Lens.lens (\ListTemplateVersions' {pageSize} -> pageSize) (\s@ListTemplateVersions' {} a -> s {pageSize = a} :: ListTemplateVersions)

-- | The name of the message template. A template name must start with an
-- alphanumeric character and can contain a maximum of 128 characters. The
-- characters can be alphanumeric characters, underscores (_), or hyphens
-- (-). Template names are case sensitive.
listTemplateVersions_templateName :: Lens.Lens' ListTemplateVersions Prelude.Text
listTemplateVersions_templateName = Lens.lens (\ListTemplateVersions' {templateName} -> templateName) (\s@ListTemplateVersions' {} a -> s {templateName = a} :: ListTemplateVersions)

-- | The type of channel that the message template is designed for. Valid
-- values are: EMAIL, PUSH, SMS, and VOICE.
listTemplateVersions_templateType :: Lens.Lens' ListTemplateVersions Prelude.Text
listTemplateVersions_templateType = Lens.lens (\ListTemplateVersions' {templateType} -> templateType) (\s@ListTemplateVersions' {} a -> s {templateType = a} :: ListTemplateVersions)

instance Prelude.AWSRequest ListTemplateVersions where
  type
    Rs ListTemplateVersions =
      ListTemplateVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTemplateVersionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.eitherParseJSON x)
      )

instance Prelude.Hashable ListTemplateVersions

instance Prelude.NFData ListTemplateVersions

instance Prelude.ToHeaders ListTemplateVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath ListTemplateVersions where
  toPath ListTemplateVersions' {..} =
    Prelude.mconcat
      [ "/v1/templates/",
        Prelude.toBS templateName,
        "/",
        Prelude.toBS templateType,
        "/versions"
      ]

instance Prelude.ToQuery ListTemplateVersions where
  toQuery ListTemplateVersions' {..} =
    Prelude.mconcat
      [ "next-token" Prelude.=: nextToken,
        "page-size" Prelude.=: pageSize
      ]

-- | /See:/ 'newListTemplateVersionsResponse' smart constructor.
data ListTemplateVersionsResponse = ListTemplateVersionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    templateVersionsResponse :: TemplateVersionsResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListTemplateVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'listTemplateVersionsResponse_httpStatus' - The response's http status code.
--
-- 'templateVersionsResponse', 'listTemplateVersionsResponse_templateVersionsResponse' - Undocumented member.
newListTemplateVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'templateVersionsResponse'
  TemplateVersionsResponse ->
  ListTemplateVersionsResponse
newListTemplateVersionsResponse
  pHttpStatus_
  pTemplateVersionsResponse_ =
    ListTemplateVersionsResponse'
      { httpStatus =
          pHttpStatus_,
        templateVersionsResponse =
          pTemplateVersionsResponse_
      }

-- | The response's http status code.
listTemplateVersionsResponse_httpStatus :: Lens.Lens' ListTemplateVersionsResponse Prelude.Int
listTemplateVersionsResponse_httpStatus = Lens.lens (\ListTemplateVersionsResponse' {httpStatus} -> httpStatus) (\s@ListTemplateVersionsResponse' {} a -> s {httpStatus = a} :: ListTemplateVersionsResponse)

-- | Undocumented member.
listTemplateVersionsResponse_templateVersionsResponse :: Lens.Lens' ListTemplateVersionsResponse TemplateVersionsResponse
listTemplateVersionsResponse_templateVersionsResponse = Lens.lens (\ListTemplateVersionsResponse' {templateVersionsResponse} -> templateVersionsResponse) (\s@ListTemplateVersionsResponse' {} a -> s {templateVersionsResponse = a} :: ListTemplateVersionsResponse)

instance Prelude.NFData ListTemplateVersionsResponse
