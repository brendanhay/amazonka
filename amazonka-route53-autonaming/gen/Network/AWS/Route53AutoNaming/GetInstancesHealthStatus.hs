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
-- Module      : Network.AWS.Route53AutoNaming.GetInstancesHealthStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the current health status (@Healthy@, @Unhealthy@, or @Unknown@) of
-- one or more instances that are associated with a specified service.
--
-- There is a brief delay between when you register an instance and when
-- the health status for the instance is available.
module Network.AWS.Route53AutoNaming.GetInstancesHealthStatus
  ( -- * Creating a Request
    GetInstancesHealthStatus (..),
    newGetInstancesHealthStatus,

    -- * Request Lenses
    getInstancesHealthStatus_nextToken,
    getInstancesHealthStatus_maxResults,
    getInstancesHealthStatus_instances,
    getInstancesHealthStatus_serviceId,

    -- * Destructuring the Response
    GetInstancesHealthStatusResponse (..),
    newGetInstancesHealthStatusResponse,

    -- * Response Lenses
    getInstancesHealthStatusResponse_status,
    getInstancesHealthStatusResponse_nextToken,
    getInstancesHealthStatusResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'newGetInstancesHealthStatus' smart constructor.
data GetInstancesHealthStatus = GetInstancesHealthStatus'
  { -- | For the first @GetInstancesHealthStatus@ request, omit this value.
    --
    -- If more than @MaxResults@ instances match the specified criteria, you
    -- can submit another @GetInstancesHealthStatus@ request to get the next
    -- group of results. Specify the value of @NextToken@ from the previous
    -- response in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of instances that you want AWS Cloud Map to return in
    -- the response to a @GetInstancesHealthStatus@ request. If you don\'t
    -- specify a value for @MaxResults@, AWS Cloud Map returns up to 100
    -- instances.
    maxResults :: Core.Maybe Core.Natural,
    -- | An array that contains the IDs of all the instances that you want to get
    -- the health status for.
    --
    -- If you omit @Instances@, AWS Cloud Map returns the health status for all
    -- the instances that are associated with the specified service.
    --
    -- To get the IDs for the instances that you\'ve registered by using a
    -- specified service, submit a
    -- <https://docs.aws.amazon.com/cloud-map/latest/api/API_ListInstances.html ListInstances>
    -- request.
    instances :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The ID of the service that the instance is associated with.
    serviceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetInstancesHealthStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getInstancesHealthStatus_nextToken' - For the first @GetInstancesHealthStatus@ request, omit this value.
--
-- If more than @MaxResults@ instances match the specified criteria, you
-- can submit another @GetInstancesHealthStatus@ request to get the next
-- group of results. Specify the value of @NextToken@ from the previous
-- response in the next request.
--
-- 'maxResults', 'getInstancesHealthStatus_maxResults' - The maximum number of instances that you want AWS Cloud Map to return in
-- the response to a @GetInstancesHealthStatus@ request. If you don\'t
-- specify a value for @MaxResults@, AWS Cloud Map returns up to 100
-- instances.
--
-- 'instances', 'getInstancesHealthStatus_instances' - An array that contains the IDs of all the instances that you want to get
-- the health status for.
--
-- If you omit @Instances@, AWS Cloud Map returns the health status for all
-- the instances that are associated with the specified service.
--
-- To get the IDs for the instances that you\'ve registered by using a
-- specified service, submit a
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_ListInstances.html ListInstances>
-- request.
--
-- 'serviceId', 'getInstancesHealthStatus_serviceId' - The ID of the service that the instance is associated with.
newGetInstancesHealthStatus ::
  -- | 'serviceId'
  Core.Text ->
  GetInstancesHealthStatus
newGetInstancesHealthStatus pServiceId_ =
  GetInstancesHealthStatus'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      instances = Core.Nothing,
      serviceId = pServiceId_
    }

-- | For the first @GetInstancesHealthStatus@ request, omit this value.
--
-- If more than @MaxResults@ instances match the specified criteria, you
-- can submit another @GetInstancesHealthStatus@ request to get the next
-- group of results. Specify the value of @NextToken@ from the previous
-- response in the next request.
getInstancesHealthStatus_nextToken :: Lens.Lens' GetInstancesHealthStatus (Core.Maybe Core.Text)
getInstancesHealthStatus_nextToken = Lens.lens (\GetInstancesHealthStatus' {nextToken} -> nextToken) (\s@GetInstancesHealthStatus' {} a -> s {nextToken = a} :: GetInstancesHealthStatus)

-- | The maximum number of instances that you want AWS Cloud Map to return in
-- the response to a @GetInstancesHealthStatus@ request. If you don\'t
-- specify a value for @MaxResults@, AWS Cloud Map returns up to 100
-- instances.
getInstancesHealthStatus_maxResults :: Lens.Lens' GetInstancesHealthStatus (Core.Maybe Core.Natural)
getInstancesHealthStatus_maxResults = Lens.lens (\GetInstancesHealthStatus' {maxResults} -> maxResults) (\s@GetInstancesHealthStatus' {} a -> s {maxResults = a} :: GetInstancesHealthStatus)

-- | An array that contains the IDs of all the instances that you want to get
-- the health status for.
--
-- If you omit @Instances@, AWS Cloud Map returns the health status for all
-- the instances that are associated with the specified service.
--
-- To get the IDs for the instances that you\'ve registered by using a
-- specified service, submit a
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_ListInstances.html ListInstances>
-- request.
getInstancesHealthStatus_instances :: Lens.Lens' GetInstancesHealthStatus (Core.Maybe (Core.NonEmpty Core.Text))
getInstancesHealthStatus_instances = Lens.lens (\GetInstancesHealthStatus' {instances} -> instances) (\s@GetInstancesHealthStatus' {} a -> s {instances = a} :: GetInstancesHealthStatus) Core.. Lens.mapping Lens._Coerce

-- | The ID of the service that the instance is associated with.
getInstancesHealthStatus_serviceId :: Lens.Lens' GetInstancesHealthStatus Core.Text
getInstancesHealthStatus_serviceId = Lens.lens (\GetInstancesHealthStatus' {serviceId} -> serviceId) (\s@GetInstancesHealthStatus' {} a -> s {serviceId = a} :: GetInstancesHealthStatus)

instance Core.AWSRequest GetInstancesHealthStatus where
  type
    AWSResponse GetInstancesHealthStatus =
      GetInstancesHealthStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstancesHealthStatusResponse'
            Core.<$> (x Core..?> "Status" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetInstancesHealthStatus

instance Core.NFData GetInstancesHealthStatus

instance Core.ToHeaders GetInstancesHealthStatus where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53AutoNaming_v20170314.GetInstancesHealthStatus" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetInstancesHealthStatus where
  toJSON GetInstancesHealthStatus' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Instances" Core..=) Core.<$> instances,
            Core.Just ("ServiceId" Core..= serviceId)
          ]
      )

instance Core.ToPath GetInstancesHealthStatus where
  toPath = Core.const "/"

instance Core.ToQuery GetInstancesHealthStatus where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetInstancesHealthStatusResponse' smart constructor.
data GetInstancesHealthStatusResponse = GetInstancesHealthStatusResponse'
  { -- | A complex type that contains the IDs and the health status of the
    -- instances that you specified in the @GetInstancesHealthStatus@ request.
    status :: Core.Maybe (Core.HashMap Core.Text HealthStatus),
    -- | If more than @MaxResults@ instances match the specified criteria, you
    -- can submit another @GetInstancesHealthStatus@ request to get the next
    -- group of results. Specify the value of @NextToken@ from the previous
    -- response in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetInstancesHealthStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getInstancesHealthStatusResponse_status' - A complex type that contains the IDs and the health status of the
-- instances that you specified in the @GetInstancesHealthStatus@ request.
--
-- 'nextToken', 'getInstancesHealthStatusResponse_nextToken' - If more than @MaxResults@ instances match the specified criteria, you
-- can submit another @GetInstancesHealthStatus@ request to get the next
-- group of results. Specify the value of @NextToken@ from the previous
-- response in the next request.
--
-- 'httpStatus', 'getInstancesHealthStatusResponse_httpStatus' - The response's http status code.
newGetInstancesHealthStatusResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetInstancesHealthStatusResponse
newGetInstancesHealthStatusResponse pHttpStatus_ =
  GetInstancesHealthStatusResponse'
    { status =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A complex type that contains the IDs and the health status of the
-- instances that you specified in the @GetInstancesHealthStatus@ request.
getInstancesHealthStatusResponse_status :: Lens.Lens' GetInstancesHealthStatusResponse (Core.Maybe (Core.HashMap Core.Text HealthStatus))
getInstancesHealthStatusResponse_status = Lens.lens (\GetInstancesHealthStatusResponse' {status} -> status) (\s@GetInstancesHealthStatusResponse' {} a -> s {status = a} :: GetInstancesHealthStatusResponse) Core.. Lens.mapping Lens._Coerce

-- | If more than @MaxResults@ instances match the specified criteria, you
-- can submit another @GetInstancesHealthStatus@ request to get the next
-- group of results. Specify the value of @NextToken@ from the previous
-- response in the next request.
getInstancesHealthStatusResponse_nextToken :: Lens.Lens' GetInstancesHealthStatusResponse (Core.Maybe Core.Text)
getInstancesHealthStatusResponse_nextToken = Lens.lens (\GetInstancesHealthStatusResponse' {nextToken} -> nextToken) (\s@GetInstancesHealthStatusResponse' {} a -> s {nextToken = a} :: GetInstancesHealthStatusResponse)

-- | The response's http status code.
getInstancesHealthStatusResponse_httpStatus :: Lens.Lens' GetInstancesHealthStatusResponse Core.Int
getInstancesHealthStatusResponse_httpStatus = Lens.lens (\GetInstancesHealthStatusResponse' {httpStatus} -> httpStatus) (\s@GetInstancesHealthStatusResponse' {} a -> s {httpStatus = a} :: GetInstancesHealthStatusResponse)

instance Core.NFData GetInstancesHealthStatusResponse
