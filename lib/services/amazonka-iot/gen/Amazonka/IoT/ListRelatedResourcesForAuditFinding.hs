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
-- Module      : Amazonka.IoT.ListRelatedResourcesForAuditFinding
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The related resources of an Audit finding. The following resources can
-- be returned from calling this API:
--
-- -   DEVICE_CERTIFICATE
--
-- -   CA_CERTIFICATE
--
-- -   IOT_POLICY
--
-- -   COGNITO_IDENTITY_POOL
--
-- -   CLIENT_ID
--
-- -   ACCOUNT_SETTINGS
--
-- -   ROLE_ALIAS
--
-- -   IAM_ROLE
--
-- -   ISSUER_CERTIFICATE
--
-- This API is similar to DescribeAuditFinding\'s
-- <https://docs.aws.amazon.com/iot/latest/apireference/API_DescribeAuditFinding.html RelatedResources>
-- but provides pagination and is not limited to 10 resources. When calling
-- <https://docs.aws.amazon.com/iot/latest/apireference/API_DescribeAuditFinding.html DescribeAuditFinding>
-- for the intermediate CA revoked for active device certificates check,
-- RelatedResources will not be populated. You must use this API,
-- ListRelatedResourcesForAuditFinding, to list the certificates.
module Amazonka.IoT.ListRelatedResourcesForAuditFinding
  ( -- * Creating a Request
    ListRelatedResourcesForAuditFinding (..),
    newListRelatedResourcesForAuditFinding,

    -- * Request Lenses
    listRelatedResourcesForAuditFinding_nextToken,
    listRelatedResourcesForAuditFinding_maxResults,
    listRelatedResourcesForAuditFinding_findingId,

    -- * Destructuring the Response
    ListRelatedResourcesForAuditFindingResponse (..),
    newListRelatedResourcesForAuditFindingResponse,

    -- * Response Lenses
    listRelatedResourcesForAuditFindingResponse_relatedResources,
    listRelatedResourcesForAuditFindingResponse_nextToken,
    listRelatedResourcesForAuditFindingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRelatedResourcesForAuditFinding' smart constructor.
data ListRelatedResourcesForAuditFinding = ListRelatedResourcesForAuditFinding'
  { -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The finding Id.
    findingId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRelatedResourcesForAuditFinding' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRelatedResourcesForAuditFinding_nextToken' - A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
--
-- 'maxResults', 'listRelatedResourcesForAuditFinding_maxResults' - The maximum number of results to return at one time.
--
-- 'findingId', 'listRelatedResourcesForAuditFinding_findingId' - The finding Id.
newListRelatedResourcesForAuditFinding ::
  -- | 'findingId'
  Prelude.Text ->
  ListRelatedResourcesForAuditFinding
newListRelatedResourcesForAuditFinding pFindingId_ =
  ListRelatedResourcesForAuditFinding'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      findingId = pFindingId_
    }

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
listRelatedResourcesForAuditFinding_nextToken :: Lens.Lens' ListRelatedResourcesForAuditFinding (Prelude.Maybe Prelude.Text)
listRelatedResourcesForAuditFinding_nextToken = Lens.lens (\ListRelatedResourcesForAuditFinding' {nextToken} -> nextToken) (\s@ListRelatedResourcesForAuditFinding' {} a -> s {nextToken = a} :: ListRelatedResourcesForAuditFinding)

-- | The maximum number of results to return at one time.
listRelatedResourcesForAuditFinding_maxResults :: Lens.Lens' ListRelatedResourcesForAuditFinding (Prelude.Maybe Prelude.Natural)
listRelatedResourcesForAuditFinding_maxResults = Lens.lens (\ListRelatedResourcesForAuditFinding' {maxResults} -> maxResults) (\s@ListRelatedResourcesForAuditFinding' {} a -> s {maxResults = a} :: ListRelatedResourcesForAuditFinding)

-- | The finding Id.
listRelatedResourcesForAuditFinding_findingId :: Lens.Lens' ListRelatedResourcesForAuditFinding Prelude.Text
listRelatedResourcesForAuditFinding_findingId = Lens.lens (\ListRelatedResourcesForAuditFinding' {findingId} -> findingId) (\s@ListRelatedResourcesForAuditFinding' {} a -> s {findingId = a} :: ListRelatedResourcesForAuditFinding)

instance
  Core.AWSRequest
    ListRelatedResourcesForAuditFinding
  where
  type
    AWSResponse ListRelatedResourcesForAuditFinding =
      ListRelatedResourcesForAuditFindingResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRelatedResourcesForAuditFindingResponse'
            Prelude.<$> ( x Core..?> "relatedResources"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Core..?> "nextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListRelatedResourcesForAuditFinding
  where
  hashWithSalt
    _salt
    ListRelatedResourcesForAuditFinding' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` findingId

instance
  Prelude.NFData
    ListRelatedResourcesForAuditFinding
  where
  rnf ListRelatedResourcesForAuditFinding' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf findingId

instance
  Core.ToHeaders
    ListRelatedResourcesForAuditFinding
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    ListRelatedResourcesForAuditFinding
  where
  toPath = Prelude.const "/audit/relatedResources"

instance
  Core.ToQuery
    ListRelatedResourcesForAuditFinding
  where
  toQuery ListRelatedResourcesForAuditFinding' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "findingId" Core.=: findingId
      ]

-- | /See:/ 'newListRelatedResourcesForAuditFindingResponse' smart constructor.
data ListRelatedResourcesForAuditFindingResponse = ListRelatedResourcesForAuditFindingResponse'
  { -- | The related resources.
    relatedResources :: Prelude.Maybe [RelatedResource],
    -- | A token that can be used to retrieve the next set of results, or @null@
    -- for the first API call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRelatedResourcesForAuditFindingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relatedResources', 'listRelatedResourcesForAuditFindingResponse_relatedResources' - The related resources.
--
-- 'nextToken', 'listRelatedResourcesForAuditFindingResponse_nextToken' - A token that can be used to retrieve the next set of results, or @null@
-- for the first API call.
--
-- 'httpStatus', 'listRelatedResourcesForAuditFindingResponse_httpStatus' - The response's http status code.
newListRelatedResourcesForAuditFindingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRelatedResourcesForAuditFindingResponse
newListRelatedResourcesForAuditFindingResponse
  pHttpStatus_ =
    ListRelatedResourcesForAuditFindingResponse'
      { relatedResources =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The related resources.
listRelatedResourcesForAuditFindingResponse_relatedResources :: Lens.Lens' ListRelatedResourcesForAuditFindingResponse (Prelude.Maybe [RelatedResource])
listRelatedResourcesForAuditFindingResponse_relatedResources = Lens.lens (\ListRelatedResourcesForAuditFindingResponse' {relatedResources} -> relatedResources) (\s@ListRelatedResourcesForAuditFindingResponse' {} a -> s {relatedResources = a} :: ListRelatedResourcesForAuditFindingResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that can be used to retrieve the next set of results, or @null@
-- for the first API call.
listRelatedResourcesForAuditFindingResponse_nextToken :: Lens.Lens' ListRelatedResourcesForAuditFindingResponse (Prelude.Maybe Prelude.Text)
listRelatedResourcesForAuditFindingResponse_nextToken = Lens.lens (\ListRelatedResourcesForAuditFindingResponse' {nextToken} -> nextToken) (\s@ListRelatedResourcesForAuditFindingResponse' {} a -> s {nextToken = a} :: ListRelatedResourcesForAuditFindingResponse)

-- | The response's http status code.
listRelatedResourcesForAuditFindingResponse_httpStatus :: Lens.Lens' ListRelatedResourcesForAuditFindingResponse Prelude.Int
listRelatedResourcesForAuditFindingResponse_httpStatus = Lens.lens (\ListRelatedResourcesForAuditFindingResponse' {httpStatus} -> httpStatus) (\s@ListRelatedResourcesForAuditFindingResponse' {} a -> s {httpStatus = a} :: ListRelatedResourcesForAuditFindingResponse)

instance
  Prelude.NFData
    ListRelatedResourcesForAuditFindingResponse
  where
  rnf ListRelatedResourcesForAuditFindingResponse' {..} =
    Prelude.rnf relatedResources
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
