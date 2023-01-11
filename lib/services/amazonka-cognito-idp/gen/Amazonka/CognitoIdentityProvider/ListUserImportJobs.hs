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
-- Module      : Amazonka.CognitoIdentityProvider.ListUserImportJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the user import jobs.
module Amazonka.CognitoIdentityProvider.ListUserImportJobs
  ( -- * Creating a Request
    ListUserImportJobs (..),
    newListUserImportJobs,

    -- * Request Lenses
    listUserImportJobs_paginationToken,
    listUserImportJobs_userPoolId,
    listUserImportJobs_maxResults,

    -- * Destructuring the Response
    ListUserImportJobsResponse (..),
    newListUserImportJobsResponse,

    -- * Response Lenses
    listUserImportJobsResponse_paginationToken,
    listUserImportJobsResponse_userImportJobs,
    listUserImportJobsResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to list the user import jobs.
--
-- /See:/ 'newListUserImportJobs' smart constructor.
data ListUserImportJobs = ListUserImportJobs'
  { -- | An identifier that was returned from the previous call to
    -- @ListUserImportJobs@, which can be used to return the next set of import
    -- jobs in the list.
    paginationToken :: Prelude.Maybe Prelude.Text,
    -- | The user pool ID for the user pool that the users are being imported
    -- into.
    userPoolId :: Prelude.Text,
    -- | The maximum number of import jobs you want the request to return.
    maxResults :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUserImportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'paginationToken', 'listUserImportJobs_paginationToken' - An identifier that was returned from the previous call to
-- @ListUserImportJobs@, which can be used to return the next set of import
-- jobs in the list.
--
-- 'userPoolId', 'listUserImportJobs_userPoolId' - The user pool ID for the user pool that the users are being imported
-- into.
--
-- 'maxResults', 'listUserImportJobs_maxResults' - The maximum number of import jobs you want the request to return.
newListUserImportJobs ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'maxResults'
  Prelude.Natural ->
  ListUserImportJobs
newListUserImportJobs pUserPoolId_ pMaxResults_ =
  ListUserImportJobs'
    { paginationToken =
        Prelude.Nothing,
      userPoolId = pUserPoolId_,
      maxResults = pMaxResults_
    }

-- | An identifier that was returned from the previous call to
-- @ListUserImportJobs@, which can be used to return the next set of import
-- jobs in the list.
listUserImportJobs_paginationToken :: Lens.Lens' ListUserImportJobs (Prelude.Maybe Prelude.Text)
listUserImportJobs_paginationToken = Lens.lens (\ListUserImportJobs' {paginationToken} -> paginationToken) (\s@ListUserImportJobs' {} a -> s {paginationToken = a} :: ListUserImportJobs)

-- | The user pool ID for the user pool that the users are being imported
-- into.
listUserImportJobs_userPoolId :: Lens.Lens' ListUserImportJobs Prelude.Text
listUserImportJobs_userPoolId = Lens.lens (\ListUserImportJobs' {userPoolId} -> userPoolId) (\s@ListUserImportJobs' {} a -> s {userPoolId = a} :: ListUserImportJobs)

-- | The maximum number of import jobs you want the request to return.
listUserImportJobs_maxResults :: Lens.Lens' ListUserImportJobs Prelude.Natural
listUserImportJobs_maxResults = Lens.lens (\ListUserImportJobs' {maxResults} -> maxResults) (\s@ListUserImportJobs' {} a -> s {maxResults = a} :: ListUserImportJobs)

instance Core.AWSRequest ListUserImportJobs where
  type
    AWSResponse ListUserImportJobs =
      ListUserImportJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUserImportJobsResponse'
            Prelude.<$> (x Data..?> "PaginationToken")
            Prelude.<*> (x Data..?> "UserImportJobs")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListUserImportJobs where
  hashWithSalt _salt ListUserImportJobs' {..} =
    _salt `Prelude.hashWithSalt` paginationToken
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListUserImportJobs where
  rnf ListUserImportJobs' {..} =
    Prelude.rnf paginationToken
      `Prelude.seq` Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListUserImportJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.ListUserImportJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListUserImportJobs where
  toJSON ListUserImportJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PaginationToken" Data..=)
              Prelude.<$> paginationToken,
            Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just ("MaxResults" Data..= maxResults)
          ]
      )

instance Data.ToPath ListUserImportJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListUserImportJobs where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server to the request to list the user
-- import jobs.
--
-- /See:/ 'newListUserImportJobsResponse' smart constructor.
data ListUserImportJobsResponse = ListUserImportJobsResponse'
  { -- | An identifier that can be used to return the next set of user import
    -- jobs in the list.
    paginationToken :: Prelude.Maybe Prelude.Text,
    -- | The user import jobs.
    userImportJobs :: Prelude.Maybe (Prelude.NonEmpty UserImportJobType),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUserImportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'paginationToken', 'listUserImportJobsResponse_paginationToken' - An identifier that can be used to return the next set of user import
-- jobs in the list.
--
-- 'userImportJobs', 'listUserImportJobsResponse_userImportJobs' - The user import jobs.
--
-- 'httpStatus', 'listUserImportJobsResponse_httpStatus' - The response's http status code.
newListUserImportJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListUserImportJobsResponse
newListUserImportJobsResponse pHttpStatus_ =
  ListUserImportJobsResponse'
    { paginationToken =
        Prelude.Nothing,
      userImportJobs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier that can be used to return the next set of user import
-- jobs in the list.
listUserImportJobsResponse_paginationToken :: Lens.Lens' ListUserImportJobsResponse (Prelude.Maybe Prelude.Text)
listUserImportJobsResponse_paginationToken = Lens.lens (\ListUserImportJobsResponse' {paginationToken} -> paginationToken) (\s@ListUserImportJobsResponse' {} a -> s {paginationToken = a} :: ListUserImportJobsResponse)

-- | The user import jobs.
listUserImportJobsResponse_userImportJobs :: Lens.Lens' ListUserImportJobsResponse (Prelude.Maybe (Prelude.NonEmpty UserImportJobType))
listUserImportJobsResponse_userImportJobs = Lens.lens (\ListUserImportJobsResponse' {userImportJobs} -> userImportJobs) (\s@ListUserImportJobsResponse' {} a -> s {userImportJobs = a} :: ListUserImportJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listUserImportJobsResponse_httpStatus :: Lens.Lens' ListUserImportJobsResponse Prelude.Int
listUserImportJobsResponse_httpStatus = Lens.lens (\ListUserImportJobsResponse' {httpStatus} -> httpStatus) (\s@ListUserImportJobsResponse' {} a -> s {httpStatus = a} :: ListUserImportJobsResponse)

instance Prelude.NFData ListUserImportJobsResponse where
  rnf ListUserImportJobsResponse' {..} =
    Prelude.rnf paginationToken
      `Prelude.seq` Prelude.rnf userImportJobs
      `Prelude.seq` Prelude.rnf httpStatus
