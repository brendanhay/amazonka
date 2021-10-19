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
-- Module      : Network.AWS.Route53AutoNaming.ListInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists summary information about the instances that you registered by
-- using a specified service.
--
-- This operation returns paginated results.
module Network.AWS.Route53AutoNaming.ListInstances
  ( -- * Creating a Request
    ListInstances (..),
    newListInstances,

    -- * Request Lenses
    listInstances_nextToken,
    listInstances_maxResults,
    listInstances_serviceId,

    -- * Destructuring the Response
    ListInstancesResponse (..),
    newListInstancesResponse,

    -- * Response Lenses
    listInstancesResponse_nextToken,
    listInstancesResponse_instances,
    listInstancesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'newListInstances' smart constructor.
data ListInstances = ListInstances'
  { -- | For the first @ListInstances@ request, omit this value.
    --
    -- If more than @MaxResults@ instances match the specified criteria, you
    -- can submit another @ListInstances@ request to get the next group of
    -- results. Specify the value of @NextToken@ from the previous response in
    -- the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of instances that you want Cloud Map to return in the
    -- response to a @ListInstances@ request. If you don\'t specify a value for
    -- @MaxResults@, Cloud Map returns up to 100 instances.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- 'nextToken', 'listInstances_nextToken' - For the first @ListInstances@ request, omit this value.
--
-- If more than @MaxResults@ instances match the specified criteria, you
-- can submit another @ListInstances@ request to get the next group of
-- results. Specify the value of @NextToken@ from the previous response in
-- the next request.
--
-- 'maxResults', 'listInstances_maxResults' - The maximum number of instances that you want Cloud Map to return in the
-- response to a @ListInstances@ request. If you don\'t specify a value for
-- @MaxResults@, Cloud Map returns up to 100 instances.
--
-- 'serviceId', 'listInstances_serviceId' - The ID of the service that you want to list instances for.
newListInstances ::
  -- | 'serviceId'
  Prelude.Text ->
  ListInstances
newListInstances pServiceId_ =
  ListInstances'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      serviceId = pServiceId_
    }

-- | For the first @ListInstances@ request, omit this value.
--
-- If more than @MaxResults@ instances match the specified criteria, you
-- can submit another @ListInstances@ request to get the next group of
-- results. Specify the value of @NextToken@ from the previous response in
-- the next request.
listInstances_nextToken :: Lens.Lens' ListInstances (Prelude.Maybe Prelude.Text)
listInstances_nextToken = Lens.lens (\ListInstances' {nextToken} -> nextToken) (\s@ListInstances' {} a -> s {nextToken = a} :: ListInstances)

-- | The maximum number of instances that you want Cloud Map to return in the
-- response to a @ListInstances@ request. If you don\'t specify a value for
-- @MaxResults@, Cloud Map returns up to 100 instances.
listInstances_maxResults :: Lens.Lens' ListInstances (Prelude.Maybe Prelude.Natural)
listInstances_maxResults = Lens.lens (\ListInstances' {maxResults} -> maxResults) (\s@ListInstances' {} a -> s {maxResults = a} :: ListInstances)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInstancesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Instances" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInstances

instance Prelude.NFData ListInstances

instance Core.ToHeaders ListInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53AutoNaming_v20170314.ListInstances" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListInstances where
  toJSON ListInstances' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("ServiceId" Core..= serviceId)
          ]
      )

instance Core.ToPath ListInstances where
  toPath = Prelude.const "/"

instance Core.ToQuery ListInstances where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListInstancesResponse' smart constructor.
data ListInstancesResponse = ListInstancesResponse'
  { -- | If more than @MaxResults@ instances match the specified criteria, you
    -- can submit another @ListInstances@ request to get the next group of
    -- results. Specify the value of @NextToken@ from the previous response in
    -- the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Summary information about the instances that are associated with the
    -- specified service.
    instances :: Prelude.Maybe [InstanceSummary],
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
-- 'nextToken', 'listInstancesResponse_nextToken' - If more than @MaxResults@ instances match the specified criteria, you
-- can submit another @ListInstances@ request to get the next group of
-- results. Specify the value of @NextToken@ from the previous response in
-- the next request.
--
-- 'instances', 'listInstancesResponse_instances' - Summary information about the instances that are associated with the
-- specified service.
--
-- 'httpStatus', 'listInstancesResponse_httpStatus' - The response's http status code.
newListInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInstancesResponse
newListInstancesResponse pHttpStatus_ =
  ListInstancesResponse'
    { nextToken = Prelude.Nothing,
      instances = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If more than @MaxResults@ instances match the specified criteria, you
-- can submit another @ListInstances@ request to get the next group of
-- results. Specify the value of @NextToken@ from the previous response in
-- the next request.
listInstancesResponse_nextToken :: Lens.Lens' ListInstancesResponse (Prelude.Maybe Prelude.Text)
listInstancesResponse_nextToken = Lens.lens (\ListInstancesResponse' {nextToken} -> nextToken) (\s@ListInstancesResponse' {} a -> s {nextToken = a} :: ListInstancesResponse)

-- | Summary information about the instances that are associated with the
-- specified service.
listInstancesResponse_instances :: Lens.Lens' ListInstancesResponse (Prelude.Maybe [InstanceSummary])
listInstancesResponse_instances = Lens.lens (\ListInstancesResponse' {instances} -> instances) (\s@ListInstancesResponse' {} a -> s {instances = a} :: ListInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listInstancesResponse_httpStatus :: Lens.Lens' ListInstancesResponse Prelude.Int
listInstancesResponse_httpStatus = Lens.lens (\ListInstancesResponse' {httpStatus} -> httpStatus) (\s@ListInstancesResponse' {} a -> s {httpStatus = a} :: ListInstancesResponse)

instance Prelude.NFData ListInstancesResponse
