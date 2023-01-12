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
-- Module      : Amazonka.IoT.ListProvisioningTemplateVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A list of provisioning template versions.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListProvisioningTemplateVersions>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListProvisioningTemplateVersions
  ( -- * Creating a Request
    ListProvisioningTemplateVersions (..),
    newListProvisioningTemplateVersions,

    -- * Request Lenses
    listProvisioningTemplateVersions_maxResults,
    listProvisioningTemplateVersions_nextToken,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListProvisioningTemplateVersions' smart constructor.
data ListProvisioningTemplateVersions = ListProvisioningTemplateVersions'
  { -- | The maximum number of results to return at one time.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the provisioning template.
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
-- 'maxResults', 'listProvisioningTemplateVersions_maxResults' - The maximum number of results to return at one time.
--
-- 'nextToken', 'listProvisioningTemplateVersions_nextToken' - A token to retrieve the next set of results.
--
-- 'templateName', 'listProvisioningTemplateVersions_templateName' - The name of the provisioning template.
newListProvisioningTemplateVersions ::
  -- | 'templateName'
  Prelude.Text ->
  ListProvisioningTemplateVersions
newListProvisioningTemplateVersions pTemplateName_ =
  ListProvisioningTemplateVersions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      templateName = pTemplateName_
    }

-- | The maximum number of results to return at one time.
listProvisioningTemplateVersions_maxResults :: Lens.Lens' ListProvisioningTemplateVersions (Prelude.Maybe Prelude.Natural)
listProvisioningTemplateVersions_maxResults = Lens.lens (\ListProvisioningTemplateVersions' {maxResults} -> maxResults) (\s@ListProvisioningTemplateVersions' {} a -> s {maxResults = a} :: ListProvisioningTemplateVersions)

-- | A token to retrieve the next set of results.
listProvisioningTemplateVersions_nextToken :: Lens.Lens' ListProvisioningTemplateVersions (Prelude.Maybe Prelude.Text)
listProvisioningTemplateVersions_nextToken = Lens.lens (\ListProvisioningTemplateVersions' {nextToken} -> nextToken) (\s@ListProvisioningTemplateVersions' {} a -> s {nextToken = a} :: ListProvisioningTemplateVersions)

-- | The name of the provisioning template.
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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProvisioningTemplateVersionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "versions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListProvisioningTemplateVersions
  where
  hashWithSalt
    _salt
    ListProvisioningTemplateVersions' {..} =
      _salt `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` templateName

instance
  Prelude.NFData
    ListProvisioningTemplateVersions
  where
  rnf ListProvisioningTemplateVersions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf templateName

instance
  Data.ToHeaders
    ListProvisioningTemplateVersions
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListProvisioningTemplateVersions where
  toPath ListProvisioningTemplateVersions' {..} =
    Prelude.mconcat
      [ "/provisioning-templates/",
        Data.toBS templateName,
        "/versions"
      ]

instance
  Data.ToQuery
    ListProvisioningTemplateVersions
  where
  toQuery ListProvisioningTemplateVersions' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListProvisioningTemplateVersionsResponse' smart constructor.
data ListProvisioningTemplateVersionsResponse = ListProvisioningTemplateVersionsResponse'
  { -- | A token to retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of provisioning template versions.
    versions :: Prelude.Maybe [ProvisioningTemplateVersionSummary],
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
-- 'nextToken', 'listProvisioningTemplateVersionsResponse_nextToken' - A token to retrieve the next set of results.
--
-- 'versions', 'listProvisioningTemplateVersionsResponse_versions' - The list of provisioning template versions.
--
-- 'httpStatus', 'listProvisioningTemplateVersionsResponse_httpStatus' - The response's http status code.
newListProvisioningTemplateVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProvisioningTemplateVersionsResponse
newListProvisioningTemplateVersionsResponse
  pHttpStatus_ =
    ListProvisioningTemplateVersionsResponse'
      { nextToken =
          Prelude.Nothing,
        versions = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A token to retrieve the next set of results.
listProvisioningTemplateVersionsResponse_nextToken :: Lens.Lens' ListProvisioningTemplateVersionsResponse (Prelude.Maybe Prelude.Text)
listProvisioningTemplateVersionsResponse_nextToken = Lens.lens (\ListProvisioningTemplateVersionsResponse' {nextToken} -> nextToken) (\s@ListProvisioningTemplateVersionsResponse' {} a -> s {nextToken = a} :: ListProvisioningTemplateVersionsResponse)

-- | The list of provisioning template versions.
listProvisioningTemplateVersionsResponse_versions :: Lens.Lens' ListProvisioningTemplateVersionsResponse (Prelude.Maybe [ProvisioningTemplateVersionSummary])
listProvisioningTemplateVersionsResponse_versions = Lens.lens (\ListProvisioningTemplateVersionsResponse' {versions} -> versions) (\s@ListProvisioningTemplateVersionsResponse' {} a -> s {versions = a} :: ListProvisioningTemplateVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listProvisioningTemplateVersionsResponse_httpStatus :: Lens.Lens' ListProvisioningTemplateVersionsResponse Prelude.Int
listProvisioningTemplateVersionsResponse_httpStatus = Lens.lens (\ListProvisioningTemplateVersionsResponse' {httpStatus} -> httpStatus) (\s@ListProvisioningTemplateVersionsResponse' {} a -> s {httpStatus = a} :: ListProvisioningTemplateVersionsResponse)

instance
  Prelude.NFData
    ListProvisioningTemplateVersionsResponse
  where
  rnf ListProvisioningTemplateVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf versions
      `Prelude.seq` Prelude.rnf httpStatus
