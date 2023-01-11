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
-- Module      : Amazonka.Route53AutoNaming.ListInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists summary information about the instances that you registered by
-- using a specified service.
--
-- This operation returns paginated results.
module Amazonka.Route53AutoNaming.ListInstances
  ( -- * Creating a Request
    ListInstances (..),
    newListInstances,

    -- * Request Lenses
    listInstances_maxResults,
    listInstances_nextToken,
    listInstances_serviceId,

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
import Amazonka.Route53AutoNaming.Types

-- | /See:/ 'newListInstances' smart constructor.
data ListInstances = ListInstances'
  { -- | The maximum number of instances that you want Cloud Map to return in the
    -- response to a @ListInstances@ request. If you don\'t specify a value for
    -- @MaxResults@, Cloud Map returns up to 100 instances.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | For the first @ListInstances@ request, omit this value.
    --
    -- If more than @MaxResults@ instances match the specified criteria, you
    -- can submit another @ListInstances@ request to get the next group of
    -- results. Specify the value of @NextToken@ from the previous response in
    -- the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the service that you want to list instances for.
    serviceId :: Prelude.Text
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
-- 'maxResults', 'listInstances_maxResults' - The maximum number of instances that you want Cloud Map to return in the
-- response to a @ListInstances@ request. If you don\'t specify a value for
-- @MaxResults@, Cloud Map returns up to 100 instances.
--
-- 'nextToken', 'listInstances_nextToken' - For the first @ListInstances@ request, omit this value.
--
-- If more than @MaxResults@ instances match the specified criteria, you
-- can submit another @ListInstances@ request to get the next group of
-- results. Specify the value of @NextToken@ from the previous response in
-- the next request.
--
-- 'serviceId', 'listInstances_serviceId' - The ID of the service that you want to list instances for.
newListInstances ::
  -- | 'serviceId'
  Prelude.Text ->
  ListInstances
newListInstances pServiceId_ =
  ListInstances'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      serviceId = pServiceId_
    }

-- | The maximum number of instances that you want Cloud Map to return in the
-- response to a @ListInstances@ request. If you don\'t specify a value for
-- @MaxResults@, Cloud Map returns up to 100 instances.
listInstances_maxResults :: Lens.Lens' ListInstances (Prelude.Maybe Prelude.Natural)
listInstances_maxResults = Lens.lens (\ListInstances' {maxResults} -> maxResults) (\s@ListInstances' {} a -> s {maxResults = a} :: ListInstances)

-- | For the first @ListInstances@ request, omit this value.
--
-- If more than @MaxResults@ instances match the specified criteria, you
-- can submit another @ListInstances@ request to get the next group of
-- results. Specify the value of @NextToken@ from the previous response in
-- the next request.
listInstances_nextToken :: Lens.Lens' ListInstances (Prelude.Maybe Prelude.Text)
listInstances_nextToken = Lens.lens (\ListInstances' {nextToken} -> nextToken) (\s@ListInstances' {} a -> s {nextToken = a} :: ListInstances)

-- | The ID of the service that you want to list instances for.
listInstances_serviceId :: Lens.Lens' ListInstances Prelude.Text
listInstances_serviceId = Lens.lens (\ListInstances' {serviceId} -> serviceId) (\s@ListInstances' {} a -> s {serviceId = a} :: ListInstances)

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
      `Prelude.hashWithSalt` serviceId

instance Prelude.NFData ListInstances where
  rnf ListInstances' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf serviceId

instance Data.ToHeaders ListInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53AutoNaming_v20170314.ListInstances" ::
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
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("ServiceId" Data..= serviceId)
          ]
      )

instance Data.ToPath ListInstances where
  toPath = Prelude.const "/"

instance Data.ToQuery ListInstances where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListInstancesResponse' smart constructor.
data ListInstancesResponse = ListInstancesResponse'
  { -- | Summary information about the instances that are associated with the
    -- specified service.
    instances :: Prelude.Maybe [InstanceSummary],
    -- | If more than @MaxResults@ instances match the specified criteria, you
    -- can submit another @ListInstances@ request to get the next group of
    -- results. Specify the value of @NextToken@ from the previous response in
    -- the next request.
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
-- 'instances', 'listInstancesResponse_instances' - Summary information about the instances that are associated with the
-- specified service.
--
-- 'nextToken', 'listInstancesResponse_nextToken' - If more than @MaxResults@ instances match the specified criteria, you
-- can submit another @ListInstances@ request to get the next group of
-- results. Specify the value of @NextToken@ from the previous response in
-- the next request.
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

-- | Summary information about the instances that are associated with the
-- specified service.
listInstancesResponse_instances :: Lens.Lens' ListInstancesResponse (Prelude.Maybe [InstanceSummary])
listInstancesResponse_instances = Lens.lens (\ListInstancesResponse' {instances} -> instances) (\s@ListInstancesResponse' {} a -> s {instances = a} :: ListInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If more than @MaxResults@ instances match the specified criteria, you
-- can submit another @ListInstances@ request to get the next group of
-- results. Specify the value of @NextToken@ from the previous response in
-- the next request.
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
