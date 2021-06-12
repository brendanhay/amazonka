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
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of instances that you want AWS Cloud Map to return in
    -- the response to a @ListInstances@ request. If you don\'t specify a value
    -- for @MaxResults@, AWS Cloud Map returns up to 100 instances.
    maxResults :: Core.Maybe Core.Natural,
    -- | The ID of the service that you want to list instances for.
    serviceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'maxResults', 'listInstances_maxResults' - The maximum number of instances that you want AWS Cloud Map to return in
-- the response to a @ListInstances@ request. If you don\'t specify a value
-- for @MaxResults@, AWS Cloud Map returns up to 100 instances.
--
-- 'serviceId', 'listInstances_serviceId' - The ID of the service that you want to list instances for.
newListInstances ::
  -- | 'serviceId'
  Core.Text ->
  ListInstances
newListInstances pServiceId_ =
  ListInstances'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      serviceId = pServiceId_
    }

-- | For the first @ListInstances@ request, omit this value.
--
-- If more than @MaxResults@ instances match the specified criteria, you
-- can submit another @ListInstances@ request to get the next group of
-- results. Specify the value of @NextToken@ from the previous response in
-- the next request.
listInstances_nextToken :: Lens.Lens' ListInstances (Core.Maybe Core.Text)
listInstances_nextToken = Lens.lens (\ListInstances' {nextToken} -> nextToken) (\s@ListInstances' {} a -> s {nextToken = a} :: ListInstances)

-- | The maximum number of instances that you want AWS Cloud Map to return in
-- the response to a @ListInstances@ request. If you don\'t specify a value
-- for @MaxResults@, AWS Cloud Map returns up to 100 instances.
listInstances_maxResults :: Lens.Lens' ListInstances (Core.Maybe Core.Natural)
listInstances_maxResults = Lens.lens (\ListInstances' {maxResults} -> maxResults) (\s@ListInstances' {} a -> s {maxResults = a} :: ListInstances)

-- | The ID of the service that you want to list instances for.
listInstances_serviceId :: Lens.Lens' ListInstances Core.Text
listInstances_serviceId = Lens.lens (\ListInstances' {serviceId} -> serviceId) (\s@ListInstances' {} a -> s {serviceId = a} :: ListInstances)

instance Core.AWSPager ListInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInstancesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listInstancesResponse_instances Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listInstances_nextToken
          Lens..~ rs
          Lens.^? listInstancesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListInstances where
  type
    AWSResponse ListInstances =
      ListInstancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInstancesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Instances" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListInstances

instance Core.NFData ListInstances

instance Core.ToHeaders ListInstances where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53AutoNaming_v20170314.ListInstances" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListInstances where
  toJSON ListInstances' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("ServiceId" Core..= serviceId)
          ]
      )

instance Core.ToPath ListInstances where
  toPath = Core.const "/"

instance Core.ToQuery ListInstances where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListInstancesResponse' smart constructor.
data ListInstancesResponse = ListInstancesResponse'
  { -- | If more than @MaxResults@ instances match the specified criteria, you
    -- can submit another @ListInstances@ request to get the next group of
    -- results. Specify the value of @NextToken@ from the previous response in
    -- the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | Summary information about the instances that are associated with the
    -- specified service.
    instances :: Core.Maybe [InstanceSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListInstancesResponse
newListInstancesResponse pHttpStatus_ =
  ListInstancesResponse'
    { nextToken = Core.Nothing,
      instances = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If more than @MaxResults@ instances match the specified criteria, you
-- can submit another @ListInstances@ request to get the next group of
-- results. Specify the value of @NextToken@ from the previous response in
-- the next request.
listInstancesResponse_nextToken :: Lens.Lens' ListInstancesResponse (Core.Maybe Core.Text)
listInstancesResponse_nextToken = Lens.lens (\ListInstancesResponse' {nextToken} -> nextToken) (\s@ListInstancesResponse' {} a -> s {nextToken = a} :: ListInstancesResponse)

-- | Summary information about the instances that are associated with the
-- specified service.
listInstancesResponse_instances :: Lens.Lens' ListInstancesResponse (Core.Maybe [InstanceSummary])
listInstancesResponse_instances = Lens.lens (\ListInstancesResponse' {instances} -> instances) (\s@ListInstancesResponse' {} a -> s {instances = a} :: ListInstancesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listInstancesResponse_httpStatus :: Lens.Lens' ListInstancesResponse Core.Int
listInstancesResponse_httpStatus = Lens.lens (\ListInstancesResponse' {httpStatus} -> httpStatus) (\s@ListInstancesResponse' {} a -> s {httpStatus = a} :: ListInstancesResponse)

instance Core.NFData ListInstancesResponse
