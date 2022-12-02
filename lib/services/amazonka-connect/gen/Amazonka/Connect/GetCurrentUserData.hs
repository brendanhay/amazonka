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
-- Module      : Amazonka.Connect.GetCurrentUserData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the real-time active user data from the specified Amazon Connect
-- instance.
module Amazonka.Connect.GetCurrentUserData
  ( -- * Creating a Request
    GetCurrentUserData (..),
    newGetCurrentUserData,

    -- * Request Lenses
    getCurrentUserData_nextToken,
    getCurrentUserData_maxResults,
    getCurrentUserData_instanceId,
    getCurrentUserData_filters,

    -- * Destructuring the Response
    GetCurrentUserDataResponse (..),
    newGetCurrentUserDataResponse,

    -- * Response Lenses
    getCurrentUserDataResponse_nextToken,
    getCurrentUserDataResponse_userDataList,
    getCurrentUserDataResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCurrentUserData' smart constructor.
data GetCurrentUserData = GetCurrentUserData'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | Filters up to 100 @Queues@, or up to 9 @ContactStates@. The user data is
    -- retrieved only for those users who are associated with the queues and
    -- have contacts that are in the specified @ContactState@.
    filters :: UserDataFilters
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCurrentUserData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getCurrentUserData_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'getCurrentUserData_maxResults' - The maximum number of results to return per page.
--
-- 'instanceId', 'getCurrentUserData_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'filters', 'getCurrentUserData_filters' - Filters up to 100 @Queues@, or up to 9 @ContactStates@. The user data is
-- retrieved only for those users who are associated with the queues and
-- have contacts that are in the specified @ContactState@.
newGetCurrentUserData ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'filters'
  UserDataFilters ->
  GetCurrentUserData
newGetCurrentUserData pInstanceId_ pFilters_ =
  GetCurrentUserData'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      instanceId = pInstanceId_,
      filters = pFilters_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
getCurrentUserData_nextToken :: Lens.Lens' GetCurrentUserData (Prelude.Maybe Prelude.Text)
getCurrentUserData_nextToken = Lens.lens (\GetCurrentUserData' {nextToken} -> nextToken) (\s@GetCurrentUserData' {} a -> s {nextToken = a} :: GetCurrentUserData)

-- | The maximum number of results to return per page.
getCurrentUserData_maxResults :: Lens.Lens' GetCurrentUserData (Prelude.Maybe Prelude.Natural)
getCurrentUserData_maxResults = Lens.lens (\GetCurrentUserData' {maxResults} -> maxResults) (\s@GetCurrentUserData' {} a -> s {maxResults = a} :: GetCurrentUserData)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
getCurrentUserData_instanceId :: Lens.Lens' GetCurrentUserData Prelude.Text
getCurrentUserData_instanceId = Lens.lens (\GetCurrentUserData' {instanceId} -> instanceId) (\s@GetCurrentUserData' {} a -> s {instanceId = a} :: GetCurrentUserData)

-- | Filters up to 100 @Queues@, or up to 9 @ContactStates@. The user data is
-- retrieved only for those users who are associated with the queues and
-- have contacts that are in the specified @ContactState@.
getCurrentUserData_filters :: Lens.Lens' GetCurrentUserData UserDataFilters
getCurrentUserData_filters = Lens.lens (\GetCurrentUserData' {filters} -> filters) (\s@GetCurrentUserData' {} a -> s {filters = a} :: GetCurrentUserData)

instance Core.AWSRequest GetCurrentUserData where
  type
    AWSResponse GetCurrentUserData =
      GetCurrentUserDataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCurrentUserDataResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "UserDataList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCurrentUserData where
  hashWithSalt _salt GetCurrentUserData' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` filters

instance Prelude.NFData GetCurrentUserData where
  rnf GetCurrentUserData' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf filters

instance Data.ToHeaders GetCurrentUserData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCurrentUserData where
  toJSON GetCurrentUserData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("Filters" Data..= filters)
          ]
      )

instance Data.ToPath GetCurrentUserData where
  toPath GetCurrentUserData' {..} =
    Prelude.mconcat
      ["/metrics/userdata/", Data.toBS instanceId]

instance Data.ToQuery GetCurrentUserData where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCurrentUserDataResponse' smart constructor.
data GetCurrentUserDataResponse = GetCurrentUserDataResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the user data that is returned.
    userDataList :: Prelude.Maybe [UserData],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCurrentUserDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getCurrentUserDataResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'userDataList', 'getCurrentUserDataResponse_userDataList' - A list of the user data that is returned.
--
-- 'httpStatus', 'getCurrentUserDataResponse_httpStatus' - The response's http status code.
newGetCurrentUserDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCurrentUserDataResponse
newGetCurrentUserDataResponse pHttpStatus_ =
  GetCurrentUserDataResponse'
    { nextToken =
        Prelude.Nothing,
      userDataList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
getCurrentUserDataResponse_nextToken :: Lens.Lens' GetCurrentUserDataResponse (Prelude.Maybe Prelude.Text)
getCurrentUserDataResponse_nextToken = Lens.lens (\GetCurrentUserDataResponse' {nextToken} -> nextToken) (\s@GetCurrentUserDataResponse' {} a -> s {nextToken = a} :: GetCurrentUserDataResponse)

-- | A list of the user data that is returned.
getCurrentUserDataResponse_userDataList :: Lens.Lens' GetCurrentUserDataResponse (Prelude.Maybe [UserData])
getCurrentUserDataResponse_userDataList = Lens.lens (\GetCurrentUserDataResponse' {userDataList} -> userDataList) (\s@GetCurrentUserDataResponse' {} a -> s {userDataList = a} :: GetCurrentUserDataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getCurrentUserDataResponse_httpStatus :: Lens.Lens' GetCurrentUserDataResponse Prelude.Int
getCurrentUserDataResponse_httpStatus = Lens.lens (\GetCurrentUserDataResponse' {httpStatus} -> httpStatus) (\s@GetCurrentUserDataResponse' {} a -> s {httpStatus = a} :: GetCurrentUserDataResponse)

instance Prelude.NFData GetCurrentUserDataResponse where
  rnf GetCurrentUserDataResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf userDataList
      `Prelude.seq` Prelude.rnf httpStatus
