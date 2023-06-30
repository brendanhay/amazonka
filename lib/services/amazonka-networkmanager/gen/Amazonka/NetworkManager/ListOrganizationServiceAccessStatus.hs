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
-- Module      : Amazonka.NetworkManager.ListOrganizationServiceAccessStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the status of the Service Linked Role (SLR) deployment for the
-- accounts in a given Amazon Web Services Organization.
module Amazonka.NetworkManager.ListOrganizationServiceAccessStatus
  ( -- * Creating a Request
    ListOrganizationServiceAccessStatus (..),
    newListOrganizationServiceAccessStatus,

    -- * Request Lenses
    listOrganizationServiceAccessStatus_maxResults,
    listOrganizationServiceAccessStatus_nextToken,

    -- * Destructuring the Response
    ListOrganizationServiceAccessStatusResponse (..),
    newListOrganizationServiceAccessStatusResponse,

    -- * Response Lenses
    listOrganizationServiceAccessStatusResponse_nextToken,
    listOrganizationServiceAccessStatusResponse_organizationStatus,
    listOrganizationServiceAccessStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListOrganizationServiceAccessStatus' smart constructor.
data ListOrganizationServiceAccessStatus = ListOrganizationServiceAccessStatus'
  { -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOrganizationServiceAccessStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listOrganizationServiceAccessStatus_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'listOrganizationServiceAccessStatus_nextToken' - The token for the next page of results.
newListOrganizationServiceAccessStatus ::
  ListOrganizationServiceAccessStatus
newListOrganizationServiceAccessStatus =
  ListOrganizationServiceAccessStatus'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return.
listOrganizationServiceAccessStatus_maxResults :: Lens.Lens' ListOrganizationServiceAccessStatus (Prelude.Maybe Prelude.Natural)
listOrganizationServiceAccessStatus_maxResults = Lens.lens (\ListOrganizationServiceAccessStatus' {maxResults} -> maxResults) (\s@ListOrganizationServiceAccessStatus' {} a -> s {maxResults = a} :: ListOrganizationServiceAccessStatus)

-- | The token for the next page of results.
listOrganizationServiceAccessStatus_nextToken :: Lens.Lens' ListOrganizationServiceAccessStatus (Prelude.Maybe Prelude.Text)
listOrganizationServiceAccessStatus_nextToken = Lens.lens (\ListOrganizationServiceAccessStatus' {nextToken} -> nextToken) (\s@ListOrganizationServiceAccessStatus' {} a -> s {nextToken = a} :: ListOrganizationServiceAccessStatus)

instance
  Core.AWSRequest
    ListOrganizationServiceAccessStatus
  where
  type
    AWSResponse ListOrganizationServiceAccessStatus =
      ListOrganizationServiceAccessStatusResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOrganizationServiceAccessStatusResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "OrganizationStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListOrganizationServiceAccessStatus
  where
  hashWithSalt
    _salt
    ListOrganizationServiceAccessStatus' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    ListOrganizationServiceAccessStatus
  where
  rnf ListOrganizationServiceAccessStatus' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    ListOrganizationServiceAccessStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    ListOrganizationServiceAccessStatus
  where
  toPath =
    Prelude.const "/organizations/service-access"

instance
  Data.ToQuery
    ListOrganizationServiceAccessStatus
  where
  toQuery ListOrganizationServiceAccessStatus' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListOrganizationServiceAccessStatusResponse' smart constructor.
data ListOrganizationServiceAccessStatusResponse = ListOrganizationServiceAccessStatusResponse'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Displays the status of an Amazon Web Services Organization.
    organizationStatus :: Prelude.Maybe OrganizationStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOrganizationServiceAccessStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOrganizationServiceAccessStatusResponse_nextToken' - The token for the next page of results.
--
-- 'organizationStatus', 'listOrganizationServiceAccessStatusResponse_organizationStatus' - Displays the status of an Amazon Web Services Organization.
--
-- 'httpStatus', 'listOrganizationServiceAccessStatusResponse_httpStatus' - The response's http status code.
newListOrganizationServiceAccessStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOrganizationServiceAccessStatusResponse
newListOrganizationServiceAccessStatusResponse
  pHttpStatus_ =
    ListOrganizationServiceAccessStatusResponse'
      { nextToken =
          Prelude.Nothing,
        organizationStatus =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token for the next page of results.
listOrganizationServiceAccessStatusResponse_nextToken :: Lens.Lens' ListOrganizationServiceAccessStatusResponse (Prelude.Maybe Prelude.Text)
listOrganizationServiceAccessStatusResponse_nextToken = Lens.lens (\ListOrganizationServiceAccessStatusResponse' {nextToken} -> nextToken) (\s@ListOrganizationServiceAccessStatusResponse' {} a -> s {nextToken = a} :: ListOrganizationServiceAccessStatusResponse)

-- | Displays the status of an Amazon Web Services Organization.
listOrganizationServiceAccessStatusResponse_organizationStatus :: Lens.Lens' ListOrganizationServiceAccessStatusResponse (Prelude.Maybe OrganizationStatus)
listOrganizationServiceAccessStatusResponse_organizationStatus = Lens.lens (\ListOrganizationServiceAccessStatusResponse' {organizationStatus} -> organizationStatus) (\s@ListOrganizationServiceAccessStatusResponse' {} a -> s {organizationStatus = a} :: ListOrganizationServiceAccessStatusResponse)

-- | The response's http status code.
listOrganizationServiceAccessStatusResponse_httpStatus :: Lens.Lens' ListOrganizationServiceAccessStatusResponse Prelude.Int
listOrganizationServiceAccessStatusResponse_httpStatus = Lens.lens (\ListOrganizationServiceAccessStatusResponse' {httpStatus} -> httpStatus) (\s@ListOrganizationServiceAccessStatusResponse' {} a -> s {httpStatus = a} :: ListOrganizationServiceAccessStatusResponse)

instance
  Prelude.NFData
    ListOrganizationServiceAccessStatusResponse
  where
  rnf ListOrganizationServiceAccessStatusResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf organizationStatus
      `Prelude.seq` Prelude.rnf httpStatus
