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
-- Module      : Amazonka.SSOAdmin.ListInstances
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the IAM Identity Center instances that the caller has access to.
--
-- This operation returns paginated results.
module Amazonka.SSOAdmin.ListInstances
  ( -- * Creating a Request
    ListInstances (..),
    newListInstances,

    -- * Request Lenses
    listInstances_maxResults,
    listInstances_nextToken,

    -- * Destructuring the Response
    ListInstancesResponse (..),
    newListInstancesResponse,

    -- * Response Lenses
    listInstancesResponse_instances,
    listInstancesResponse_nextToken,
    listInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSOAdmin.Types

-- | /See:/ 'newListInstances' smart constructor.
data ListInstances = ListInstances'
  { -- | The maximum number of results to display for the instance.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token for the list API. Initially the value is null. Use
    -- the output of previous API calls to make subsequent calls.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listInstances_maxResults' - The maximum number of results to display for the instance.
--
-- 'nextToken', 'listInstances_nextToken' - The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
newListInstances ::
  ListInstances
newListInstances =
  ListInstances'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to display for the instance.
listInstances_maxResults :: Lens.Lens' ListInstances (Prelude.Maybe Prelude.Natural)
listInstances_maxResults = Lens.lens (\ListInstances' {maxResults} -> maxResults) (\s@ListInstances' {} a -> s {maxResults = a} :: ListInstances)

-- | The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
listInstances_nextToken :: Lens.Lens' ListInstances (Prelude.Maybe Prelude.Text)
listInstances_nextToken = Lens.lens (\ListInstances' {nextToken} -> nextToken) (\s@ListInstances' {} a -> s {nextToken = a} :: ListInstances)

instance Core.AWSPager ListInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInstancesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listInstancesResponse_instances Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listInstances_nextToken
          Lens..~ rs
          Lens.^? listInstancesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListInstances where
  type
    AWSResponse ListInstances =
      ListInstancesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInstancesResponse'
            Prelude.<$> (x Data..?> "Instances" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInstances where
  hashWithSalt _salt ListInstances' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListInstances where
  rnf ListInstances' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SWBExternalService.ListInstances" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListInstances where
  toJSON ListInstances' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListInstances where
  toPath = Prelude.const "/"

instance Data.ToQuery ListInstances where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListInstancesResponse' smart constructor.
data ListInstancesResponse = ListInstancesResponse'
  { -- | Lists the IAM Identity Center instances that the caller has access to.
    instances :: Prelude.Maybe [InstanceMetadata],
    -- | The pagination token for the list API. Initially the value is null. Use
    -- the output of previous API calls to make subsequent calls.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instances', 'listInstancesResponse_instances' - Lists the IAM Identity Center instances that the caller has access to.
--
-- 'nextToken', 'listInstancesResponse_nextToken' - The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
--
-- 'httpStatus', 'listInstancesResponse_httpStatus' - The response's http status code.
newListInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInstancesResponse
newListInstancesResponse pHttpStatus_ =
  ListInstancesResponse'
    { instances = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Lists the IAM Identity Center instances that the caller has access to.
listInstancesResponse_instances :: Lens.Lens' ListInstancesResponse (Prelude.Maybe [InstanceMetadata])
listInstancesResponse_instances = Lens.lens (\ListInstancesResponse' {instances} -> instances) (\s@ListInstancesResponse' {} a -> s {instances = a} :: ListInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token for the list API. Initially the value is null. Use
-- the output of previous API calls to make subsequent calls.
listInstancesResponse_nextToken :: Lens.Lens' ListInstancesResponse (Prelude.Maybe Prelude.Text)
listInstancesResponse_nextToken = Lens.lens (\ListInstancesResponse' {nextToken} -> nextToken) (\s@ListInstancesResponse' {} a -> s {nextToken = a} :: ListInstancesResponse)

-- | The response's http status code.
listInstancesResponse_httpStatus :: Lens.Lens' ListInstancesResponse Prelude.Int
listInstancesResponse_httpStatus = Lens.lens (\ListInstancesResponse' {httpStatus} -> httpStatus) (\s@ListInstancesResponse' {} a -> s {httpStatus = a} :: ListInstancesResponse)

instance Prelude.NFData ListInstancesResponse where
  rnf ListInstancesResponse' {..} =
    Prelude.rnf instances
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
