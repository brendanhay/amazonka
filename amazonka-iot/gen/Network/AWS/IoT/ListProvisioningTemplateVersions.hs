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
    listProvisioningTemplateVersionsResponse_nextToken,
    listProvisioningTemplateVersionsResponse_versions,
    listProvisioningTemplateVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListProvisioningTemplateVersions' smart constructor.
data ListProvisioningTemplateVersions = ListProvisioningTemplateVersions'
  { -- | A token to retrieve the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Core.Maybe Core.Natural,
    -- | The name of the fleet provisioning template.
    templateName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ListProvisioningTemplateVersions
newListProvisioningTemplateVersions pTemplateName_ =
  ListProvisioningTemplateVersions'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      templateName = pTemplateName_
    }

-- | A token to retrieve the next set of results.
listProvisioningTemplateVersions_nextToken :: Lens.Lens' ListProvisioningTemplateVersions (Core.Maybe Core.Text)
listProvisioningTemplateVersions_nextToken = Lens.lens (\ListProvisioningTemplateVersions' {nextToken} -> nextToken) (\s@ListProvisioningTemplateVersions' {} a -> s {nextToken = a} :: ListProvisioningTemplateVersions)

-- | The maximum number of results to return at one time.
listProvisioningTemplateVersions_maxResults :: Lens.Lens' ListProvisioningTemplateVersions (Core.Maybe Core.Natural)
listProvisioningTemplateVersions_maxResults = Lens.lens (\ListProvisioningTemplateVersions' {maxResults} -> maxResults) (\s@ListProvisioningTemplateVersions' {} a -> s {maxResults = a} :: ListProvisioningTemplateVersions)

-- | The name of the fleet provisioning template.
listProvisioningTemplateVersions_templateName :: Lens.Lens' ListProvisioningTemplateVersions Core.Text
listProvisioningTemplateVersions_templateName = Lens.lens (\ListProvisioningTemplateVersions' {templateName} -> templateName) (\s@ListProvisioningTemplateVersions' {} a -> s {templateName = a} :: ListProvisioningTemplateVersions)

instance
  Core.AWSPager
    ListProvisioningTemplateVersions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProvisioningTemplateVersionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listProvisioningTemplateVersionsResponse_versions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listProvisioningTemplateVersions_nextToken
          Lens..~ rs
          Lens.^? listProvisioningTemplateVersionsResponse_nextToken
            Core.. Lens._Just

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
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "versions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListProvisioningTemplateVersions

instance Core.NFData ListProvisioningTemplateVersions

instance
  Core.ToHeaders
    ListProvisioningTemplateVersions
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListProvisioningTemplateVersions where
  toPath ListProvisioningTemplateVersions' {..} =
    Core.mconcat
      [ "/provisioning-templates/",
        Core.toBS templateName,
        "/versions"
      ]

instance
  Core.ToQuery
    ListProvisioningTemplateVersions
  where
  toQuery ListProvisioningTemplateVersions' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListProvisioningTemplateVersionsResponse' smart constructor.
data ListProvisioningTemplateVersionsResponse = ListProvisioningTemplateVersionsResponse'
  { -- | A token to retrieve the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of fleet provisioning template versions.
    versions :: Core.Maybe [ProvisioningTemplateVersionSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListProvisioningTemplateVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProvisioningTemplateVersionsResponse_nextToken' - A token to retrieve the next set of results.
--
-- 'versions', 'listProvisioningTemplateVersionsResponse_versions' - The list of fleet provisioning template versions.
--
-- 'httpStatus', 'listProvisioningTemplateVersionsResponse_httpStatus' - The response's http status code.
newListProvisioningTemplateVersionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListProvisioningTemplateVersionsResponse
newListProvisioningTemplateVersionsResponse
  pHttpStatus_ =
    ListProvisioningTemplateVersionsResponse'
      { nextToken =
          Core.Nothing,
        versions = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A token to retrieve the next set of results.
listProvisioningTemplateVersionsResponse_nextToken :: Lens.Lens' ListProvisioningTemplateVersionsResponse (Core.Maybe Core.Text)
listProvisioningTemplateVersionsResponse_nextToken = Lens.lens (\ListProvisioningTemplateVersionsResponse' {nextToken} -> nextToken) (\s@ListProvisioningTemplateVersionsResponse' {} a -> s {nextToken = a} :: ListProvisioningTemplateVersionsResponse)

-- | The list of fleet provisioning template versions.
listProvisioningTemplateVersionsResponse_versions :: Lens.Lens' ListProvisioningTemplateVersionsResponse (Core.Maybe [ProvisioningTemplateVersionSummary])
listProvisioningTemplateVersionsResponse_versions = Lens.lens (\ListProvisioningTemplateVersionsResponse' {versions} -> versions) (\s@ListProvisioningTemplateVersionsResponse' {} a -> s {versions = a} :: ListProvisioningTemplateVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listProvisioningTemplateVersionsResponse_httpStatus :: Lens.Lens' ListProvisioningTemplateVersionsResponse Core.Int
listProvisioningTemplateVersionsResponse_httpStatus = Lens.lens (\ListProvisioningTemplateVersionsResponse' {httpStatus} -> httpStatus) (\s@ListProvisioningTemplateVersionsResponse' {} a -> s {httpStatus = a} :: ListProvisioningTemplateVersionsResponse)

instance
  Core.NFData
    ListProvisioningTemplateVersionsResponse
