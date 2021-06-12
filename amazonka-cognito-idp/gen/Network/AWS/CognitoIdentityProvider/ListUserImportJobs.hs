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
-- Module      : Network.AWS.CognitoIdentityProvider.ListUserImportJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the user import jobs.
module Network.AWS.CognitoIdentityProvider.ListUserImportJobs
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
    listUserImportJobsResponse_userImportJobs,
    listUserImportJobsResponse_paginationToken,
    listUserImportJobsResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to list the user import jobs.
--
-- /See:/ 'newListUserImportJobs' smart constructor.
data ListUserImportJobs = ListUserImportJobs'
  { -- | An identifier that was returned from the previous call to
    -- @ListUserImportJobs@, which can be used to return the next set of import
    -- jobs in the list.
    paginationToken :: Core.Maybe Core.Text,
    -- | The user pool ID for the user pool that the users are being imported
    -- into.
    userPoolId :: Core.Text,
    -- | The maximum number of import jobs you want the request to return.
    maxResults :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'maxResults'
  Core.Natural ->
  ListUserImportJobs
newListUserImportJobs pUserPoolId_ pMaxResults_ =
  ListUserImportJobs'
    { paginationToken = Core.Nothing,
      userPoolId = pUserPoolId_,
      maxResults = pMaxResults_
    }

-- | An identifier that was returned from the previous call to
-- @ListUserImportJobs@, which can be used to return the next set of import
-- jobs in the list.
listUserImportJobs_paginationToken :: Lens.Lens' ListUserImportJobs (Core.Maybe Core.Text)
listUserImportJobs_paginationToken = Lens.lens (\ListUserImportJobs' {paginationToken} -> paginationToken) (\s@ListUserImportJobs' {} a -> s {paginationToken = a} :: ListUserImportJobs)

-- | The user pool ID for the user pool that the users are being imported
-- into.
listUserImportJobs_userPoolId :: Lens.Lens' ListUserImportJobs Core.Text
listUserImportJobs_userPoolId = Lens.lens (\ListUserImportJobs' {userPoolId} -> userPoolId) (\s@ListUserImportJobs' {} a -> s {userPoolId = a} :: ListUserImportJobs)

-- | The maximum number of import jobs you want the request to return.
listUserImportJobs_maxResults :: Lens.Lens' ListUserImportJobs Core.Natural
listUserImportJobs_maxResults = Lens.lens (\ListUserImportJobs' {maxResults} -> maxResults) (\s@ListUserImportJobs' {} a -> s {maxResults = a} :: ListUserImportJobs)

instance Core.AWSRequest ListUserImportJobs where
  type
    AWSResponse ListUserImportJobs =
      ListUserImportJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUserImportJobsResponse'
            Core.<$> (x Core..?> "UserImportJobs")
            Core.<*> (x Core..?> "PaginationToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListUserImportJobs

instance Core.NFData ListUserImportJobs

instance Core.ToHeaders ListUserImportJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.ListUserImportJobs" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListUserImportJobs where
  toJSON ListUserImportJobs' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PaginationToken" Core..=)
              Core.<$> paginationToken,
            Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("MaxResults" Core..= maxResults)
          ]
      )

instance Core.ToPath ListUserImportJobs where
  toPath = Core.const "/"

instance Core.ToQuery ListUserImportJobs where
  toQuery = Core.const Core.mempty

-- | Represents the response from the server to the request to list the user
-- import jobs.
--
-- /See:/ 'newListUserImportJobsResponse' smart constructor.
data ListUserImportJobsResponse = ListUserImportJobsResponse'
  { -- | The user import jobs.
    userImportJobs :: Core.Maybe (Core.NonEmpty UserImportJobType),
    -- | An identifier that can be used to return the next set of user import
    -- jobs in the list.
    paginationToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListUserImportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userImportJobs', 'listUserImportJobsResponse_userImportJobs' - The user import jobs.
--
-- 'paginationToken', 'listUserImportJobsResponse_paginationToken' - An identifier that can be used to return the next set of user import
-- jobs in the list.
--
-- 'httpStatus', 'listUserImportJobsResponse_httpStatus' - The response's http status code.
newListUserImportJobsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListUserImportJobsResponse
newListUserImportJobsResponse pHttpStatus_ =
  ListUserImportJobsResponse'
    { userImportJobs =
        Core.Nothing,
      paginationToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user import jobs.
listUserImportJobsResponse_userImportJobs :: Lens.Lens' ListUserImportJobsResponse (Core.Maybe (Core.NonEmpty UserImportJobType))
listUserImportJobsResponse_userImportJobs = Lens.lens (\ListUserImportJobsResponse' {userImportJobs} -> userImportJobs) (\s@ListUserImportJobsResponse' {} a -> s {userImportJobs = a} :: ListUserImportJobsResponse) Core.. Lens.mapping Lens._Coerce

-- | An identifier that can be used to return the next set of user import
-- jobs in the list.
listUserImportJobsResponse_paginationToken :: Lens.Lens' ListUserImportJobsResponse (Core.Maybe Core.Text)
listUserImportJobsResponse_paginationToken = Lens.lens (\ListUserImportJobsResponse' {paginationToken} -> paginationToken) (\s@ListUserImportJobsResponse' {} a -> s {paginationToken = a} :: ListUserImportJobsResponse)

-- | The response's http status code.
listUserImportJobsResponse_httpStatus :: Lens.Lens' ListUserImportJobsResponse Core.Int
listUserImportJobsResponse_httpStatus = Lens.lens (\ListUserImportJobsResponse' {httpStatus} -> httpStatus) (\s@ListUserImportJobsResponse' {} a -> s {httpStatus = a} :: ListUserImportJobsResponse)

instance Core.NFData ListUserImportJobsResponse
