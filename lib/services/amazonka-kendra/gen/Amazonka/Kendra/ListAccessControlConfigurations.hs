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
-- Module      : Amazonka.Kendra.ListAccessControlConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists one or more access control configurations for an index. This
-- includes user and group access information for your documents. This is
-- useful for user context filtering, where search results are filtered
-- based on the user or their group access to documents.
module Amazonka.Kendra.ListAccessControlConfigurations
  ( -- * Creating a Request
    ListAccessControlConfigurations (..),
    newListAccessControlConfigurations,

    -- * Request Lenses
    listAccessControlConfigurations_nextToken,
    listAccessControlConfigurations_maxResults,
    listAccessControlConfigurations_indexId,

    -- * Destructuring the Response
    ListAccessControlConfigurationsResponse (..),
    newListAccessControlConfigurationsResponse,

    -- * Response Lenses
    listAccessControlConfigurationsResponse_nextToken,
    listAccessControlConfigurationsResponse_httpStatus,
    listAccessControlConfigurationsResponse_accessControlConfigurations,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAccessControlConfigurations' smart constructor.
data ListAccessControlConfigurations = ListAccessControlConfigurations'
  { -- | If the previous response was incomplete (because there\'s more data to
    -- retrieve), Amazon Kendra returns a pagination token in the response. You
    -- can use this pagination token to retrieve the next set of access control
    -- configurations.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of access control configurations to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the index for the access control configuration.
    indexId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccessControlConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAccessControlConfigurations_nextToken' - If the previous response was incomplete (because there\'s more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of access control
-- configurations.
--
-- 'maxResults', 'listAccessControlConfigurations_maxResults' - The maximum number of access control configurations to return.
--
-- 'indexId', 'listAccessControlConfigurations_indexId' - The identifier of the index for the access control configuration.
newListAccessControlConfigurations ::
  -- | 'indexId'
  Prelude.Text ->
  ListAccessControlConfigurations
newListAccessControlConfigurations pIndexId_ =
  ListAccessControlConfigurations'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      indexId = pIndexId_
    }

-- | If the previous response was incomplete (because there\'s more data to
-- retrieve), Amazon Kendra returns a pagination token in the response. You
-- can use this pagination token to retrieve the next set of access control
-- configurations.
listAccessControlConfigurations_nextToken :: Lens.Lens' ListAccessControlConfigurations (Prelude.Maybe Prelude.Text)
listAccessControlConfigurations_nextToken = Lens.lens (\ListAccessControlConfigurations' {nextToken} -> nextToken) (\s@ListAccessControlConfigurations' {} a -> s {nextToken = a} :: ListAccessControlConfigurations)

-- | The maximum number of access control configurations to return.
listAccessControlConfigurations_maxResults :: Lens.Lens' ListAccessControlConfigurations (Prelude.Maybe Prelude.Natural)
listAccessControlConfigurations_maxResults = Lens.lens (\ListAccessControlConfigurations' {maxResults} -> maxResults) (\s@ListAccessControlConfigurations' {} a -> s {maxResults = a} :: ListAccessControlConfigurations)

-- | The identifier of the index for the access control configuration.
listAccessControlConfigurations_indexId :: Lens.Lens' ListAccessControlConfigurations Prelude.Text
listAccessControlConfigurations_indexId = Lens.lens (\ListAccessControlConfigurations' {indexId} -> indexId) (\s@ListAccessControlConfigurations' {} a -> s {indexId = a} :: ListAccessControlConfigurations)

instance
  Core.AWSRequest
    ListAccessControlConfigurations
  where
  type
    AWSResponse ListAccessControlConfigurations =
      ListAccessControlConfigurationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAccessControlConfigurationsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "AccessControlConfigurations"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ListAccessControlConfigurations
  where
  hashWithSalt
    _salt
    ListAccessControlConfigurations' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` indexId

instance
  Prelude.NFData
    ListAccessControlConfigurations
  where
  rnf ListAccessControlConfigurations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf indexId

instance
  Core.ToHeaders
    ListAccessControlConfigurations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSKendraFrontendService.ListAccessControlConfigurations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListAccessControlConfigurations where
  toJSON ListAccessControlConfigurations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("IndexId" Core..= indexId)
          ]
      )

instance Core.ToPath ListAccessControlConfigurations where
  toPath = Prelude.const "/"

instance Core.ToQuery ListAccessControlConfigurations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAccessControlConfigurationsResponse' smart constructor.
data ListAccessControlConfigurationsResponse = ListAccessControlConfigurationsResponse'
  { -- | If the response is truncated, Amazon Kendra returns this token, which
    -- you can use in the subsequent request to retrieve the next set of access
    -- control configurations.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The details of your access control configurations.
    accessControlConfigurations :: [AccessControlConfigurationSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAccessControlConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAccessControlConfigurationsResponse_nextToken' - If the response is truncated, Amazon Kendra returns this token, which
-- you can use in the subsequent request to retrieve the next set of access
-- control configurations.
--
-- 'httpStatus', 'listAccessControlConfigurationsResponse_httpStatus' - The response's http status code.
--
-- 'accessControlConfigurations', 'listAccessControlConfigurationsResponse_accessControlConfigurations' - The details of your access control configurations.
newListAccessControlConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAccessControlConfigurationsResponse
newListAccessControlConfigurationsResponse
  pHttpStatus_ =
    ListAccessControlConfigurationsResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        accessControlConfigurations =
          Prelude.mempty
      }

-- | If the response is truncated, Amazon Kendra returns this token, which
-- you can use in the subsequent request to retrieve the next set of access
-- control configurations.
listAccessControlConfigurationsResponse_nextToken :: Lens.Lens' ListAccessControlConfigurationsResponse (Prelude.Maybe Prelude.Text)
listAccessControlConfigurationsResponse_nextToken = Lens.lens (\ListAccessControlConfigurationsResponse' {nextToken} -> nextToken) (\s@ListAccessControlConfigurationsResponse' {} a -> s {nextToken = a} :: ListAccessControlConfigurationsResponse)

-- | The response's http status code.
listAccessControlConfigurationsResponse_httpStatus :: Lens.Lens' ListAccessControlConfigurationsResponse Prelude.Int
listAccessControlConfigurationsResponse_httpStatus = Lens.lens (\ListAccessControlConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListAccessControlConfigurationsResponse' {} a -> s {httpStatus = a} :: ListAccessControlConfigurationsResponse)

-- | The details of your access control configurations.
listAccessControlConfigurationsResponse_accessControlConfigurations :: Lens.Lens' ListAccessControlConfigurationsResponse [AccessControlConfigurationSummary]
listAccessControlConfigurationsResponse_accessControlConfigurations = Lens.lens (\ListAccessControlConfigurationsResponse' {accessControlConfigurations} -> accessControlConfigurations) (\s@ListAccessControlConfigurationsResponse' {} a -> s {accessControlConfigurations = a} :: ListAccessControlConfigurationsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListAccessControlConfigurationsResponse
  where
  rnf ListAccessControlConfigurationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf accessControlConfigurations
