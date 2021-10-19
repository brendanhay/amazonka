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
-- Module      : Network.AWS.IoT.ListProvisioningTemplateVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A list of fleet provisioning template versions.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListProvisioningTemplateVersions>
-- action.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListProvisioningTemplateVersions
  ( -- * Creating a Request
    ListProvisioningTemplateVersions (..),
    newListProvisioningTemplateVersions,

    -- * Request Lenses
    listProvisioningTemplateVersions_nextToken,
    listProvisioningTemplateVersions_maxResults,
    listProvisioningTemplateVersions_templateName,

    -- * Destructuring the Response
    ListProvisioningTemplateVersionsResponse (..),
    newListProvisioningTemplateVersionsResponse,

    -- * Response Lenses
    listProvisioningTemplateVersionsResponse_versions,
    listProvisioningTemplateVersionsResponse_nextToken,
    listProvisioningTemplateVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListProvisioningTemplateVersions' smart constructor.
data ListProvisioningTemplateVersions = ListProvisioningTemplateVersions'
  { -- | A token to retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the fleet provisioning template.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProvisioningTemplateVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProvisioningTemplateVersions_nextToken' - A token to retrieve the next set of results.
--
-- 'maxResults', 'listProvisioningTemplateVersions_maxResults' - The maximum number of results to return at one time.
--
-- 'templateName', 'listProvisioningTemplateVersions_templateName' - The name of the fleet provisioning template.
newListProvisioningTemplateVersions ::
  -- | 'templateName'
  Prelude.Text ->
  ListProvisioningTemplateVersions
newListProvisioningTemplateVersions pTemplateName_ =
  ListProvisioningTemplateVersions'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      templateName = pTemplateName_
    }

-- | A token to retrieve the next set of results.
listProvisioningTemplateVersions_nextToken :: Lens.Lens' ListProvisioningTemplateVersions (Prelude.Maybe Prelude.Text)
listProvisioningTemplateVersions_nextToken = Lens.lens (\ListProvisioningTemplateVersions' {nextToken} -> nextToken) (\s@ListProvisioningTemplateVersions' {} a -> s {nextToken = a} :: ListProvisioningTemplateVersions)

-- | The maximum number of results to return at one time.
listProvisioningTemplateVersions_maxResults :: Lens.Lens' ListProvisioningTemplateVersions (Prelude.Maybe Prelude.Natural)
listProvisioningTemplateVersions_maxResults = Lens.lens (\ListProvisioningTemplateVersions' {maxResults} -> maxResults) (\s@ListProvisioningTemplateVersions' {} a -> s {maxResults = a} :: ListProvisioningTemplateVersions)

-- | The name of the fleet provisioning template.
listProvisioningTemplateVersions_templateName :: Lens.Lens' ListProvisioningTemplateVersions Prelude.Text
listProvisioningTemplateVersions_templateName = Lens.lens (\ListProvisioningTemplateVersions' {templateName} -> templateName) (\s@ListProvisioningTemplateVersions' {} a -> s {templateName = a} :: ListProvisioningTemplateVersions)

instance
  Core.AWSPager
    ListProvisioningTemplateVersions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProvisioningTemplateVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listProvisioningTemplateVersionsResponse_versions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listProvisioningTemplateVersions_nextToken
          Lens..~ rs
          Lens.^? listProvisioningTemplateVersionsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListProvisioningTemplateVersions
  where
  type
    AWSResponse ListProvisioningTemplateVersions =
      ListProvisioningTemplateVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProvisioningTemplateVersionsResponse'
            Prelude.<$> (x Core..?> "versions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListProvisioningTemplateVersions

instance
  Prelude.NFData
    ListProvisioningTemplateVersions

instance
  Core.ToHeaders
    ListProvisioningTemplateVersions
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListProvisioningTemplateVersions where
  toPath ListProvisioningTemplateVersions' {..} =
    Prelude.mconcat
      [ "/provisioning-templates/",
        Core.toBS templateName,
        "/versions"
      ]

instance
  Core.ToQuery
    ListProvisioningTemplateVersions
  where
  toQuery ListProvisioningTemplateVersions' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListProvisioningTemplateVersionsResponse' smart constructor.
data ListProvisioningTemplateVersionsResponse = ListProvisioningTemplateVersionsResponse'
  { -- | The list of fleet provisioning template versions.
    versions :: Prelude.Maybe [ProvisioningTemplateVersionSummary],
    -- | A token to retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProvisioningTemplateVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versions', 'listProvisioningTemplateVersionsResponse_versions' - The list of fleet provisioning template versions.
--
-- 'nextToken', 'listProvisioningTemplateVersionsResponse_nextToken' - A token to retrieve the next set of results.
--
-- 'httpStatus', 'listProvisioningTemplateVersionsResponse_httpStatus' - The response's http status code.
newListProvisioningTemplateVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProvisioningTemplateVersionsResponse
newListProvisioningTemplateVersionsResponse
  pHttpStatus_ =
    ListProvisioningTemplateVersionsResponse'
      { versions =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The list of fleet provisioning template versions.
listProvisioningTemplateVersionsResponse_versions :: Lens.Lens' ListProvisioningTemplateVersionsResponse (Prelude.Maybe [ProvisioningTemplateVersionSummary])
listProvisioningTemplateVersionsResponse_versions = Lens.lens (\ListProvisioningTemplateVersionsResponse' {versions} -> versions) (\s@ListProvisioningTemplateVersionsResponse' {} a -> s {versions = a} :: ListProvisioningTemplateVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token to retrieve the next set of results.
listProvisioningTemplateVersionsResponse_nextToken :: Lens.Lens' ListProvisioningTemplateVersionsResponse (Prelude.Maybe Prelude.Text)
listProvisioningTemplateVersionsResponse_nextToken = Lens.lens (\ListProvisioningTemplateVersionsResponse' {nextToken} -> nextToken) (\s@ListProvisioningTemplateVersionsResponse' {} a -> s {nextToken = a} :: ListProvisioningTemplateVersionsResponse)

-- | The response's http status code.
listProvisioningTemplateVersionsResponse_httpStatus :: Lens.Lens' ListProvisioningTemplateVersionsResponse Prelude.Int
listProvisioningTemplateVersionsResponse_httpStatus = Lens.lens (\ListProvisioningTemplateVersionsResponse' {httpStatus} -> httpStatus) (\s@ListProvisioningTemplateVersionsResponse' {} a -> s {httpStatus = a} :: ListProvisioningTemplateVersionsResponse)

instance
  Prelude.NFData
    ListProvisioningTemplateVersionsResponse
