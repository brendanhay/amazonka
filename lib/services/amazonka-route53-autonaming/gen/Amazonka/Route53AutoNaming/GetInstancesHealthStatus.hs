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
-- Module      : Amazonka.Route53AutoNaming.GetInstancesHealthStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the current health status (@Healthy@, @Unhealthy@, or @Unknown@) of
-- one or more instances that are associated with a specified service.
--
-- There\'s a brief delay between when you register an instance and when
-- the health status for the instance is available.
module Amazonka.Route53AutoNaming.GetInstancesHealthStatus
  ( -- * Creating a Request
    GetInstancesHealthStatus (..),
    newGetInstancesHealthStatus,

    -- * Request Lenses
    getInstancesHealthStatus_instances,
    getInstancesHealthStatus_maxResults,
    getInstancesHealthStatus_nextToken,
    getInstancesHealthStatus_serviceId,

    -- * Destructuring the Response
    GetInstancesHealthStatusResponse (..),
    newGetInstancesHealthStatusResponse,

    -- * Response Lenses
    getInstancesHealthStatusResponse_nextToken,
    getInstancesHealthStatusResponse_status,
    getInstancesHealthStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53AutoNaming.Types

-- | /See:/ 'newGetInstancesHealthStatus' smart constructor.
data GetInstancesHealthStatus = GetInstancesHealthStatus'
  { -- | An array that contains the IDs of all the instances that you want to get
    -- the health status for.
    --
    -- If you omit @Instances@, Cloud Map returns the health status for all the
    -- instances that are associated with the specified service.
    --
    -- To get the IDs for the instances that you\'ve registered by using a
    -- specified service, submit a
    -- <https://docs.aws.amazon.com/cloud-map/latest/api/API_ListInstances.html ListInstances>
    -- request.
    instances :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The maximum number of instances that you want Cloud Map to return in the
    -- response to a @GetInstancesHealthStatus@ request. If you don\'t specify
    -- a value for @MaxResults@, Cloud Map returns up to 100 instances.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | For the first @GetInstancesHealthStatus@ request, omit this value.
    --
    -- If more than @MaxResults@ instances match the specified criteria, you
    -- can submit another @GetInstancesHealthStatus@ request to get the next
    -- group of results. Specify the value of @NextToken@ from the previous
    -- response in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the service that the instance is associated with.
    serviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInstancesHealthStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instances', 'getInstancesHealthStatus_instances' - An array that contains the IDs of all the instances that you want to get
-- the health status for.
--
-- If you omit @Instances@, Cloud Map returns the health status for all the
-- instances that are associated with the specified service.
--
-- To get the IDs for the instances that you\'ve registered by using a
-- specified service, submit a
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_ListInstances.html ListInstances>
-- request.
--
-- 'maxResults', 'getInstancesHealthStatus_maxResults' - The maximum number of instances that you want Cloud Map to return in the
-- response to a @GetInstancesHealthStatus@ request. If you don\'t specify
-- a value for @MaxResults@, Cloud Map returns up to 100 instances.
--
-- 'nextToken', 'getInstancesHealthStatus_nextToken' - For the first @GetInstancesHealthStatus@ request, omit this value.
--
-- If more than @MaxResults@ instances match the specified criteria, you
-- can submit another @GetInstancesHealthStatus@ request to get the next
-- group of results. Specify the value of @NextToken@ from the previous
-- response in the next request.
--
-- 'serviceId', 'getInstancesHealthStatus_serviceId' - The ID of the service that the instance is associated with.
newGetInstancesHealthStatus ::
  -- | 'serviceId'
  Prelude.Text ->
  GetInstancesHealthStatus
newGetInstancesHealthStatus pServiceId_ =
  GetInstancesHealthStatus'
    { instances =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      serviceId = pServiceId_
    }

-- | An array that contains the IDs of all the instances that you want to get
-- the health status for.
--
-- If you omit @Instances@, Cloud Map returns the health status for all the
-- instances that are associated with the specified service.
--
-- To get the IDs for the instances that you\'ve registered by using a
-- specified service, submit a
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_ListInstances.html ListInstances>
-- request.
getInstancesHealthStatus_instances :: Lens.Lens' GetInstancesHealthStatus (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
getInstancesHealthStatus_instances = Lens.lens (\GetInstancesHealthStatus' {instances} -> instances) (\s@GetInstancesHealthStatus' {} a -> s {instances = a} :: GetInstancesHealthStatus) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of instances that you want Cloud Map to return in the
-- response to a @GetInstancesHealthStatus@ request. If you don\'t specify
-- a value for @MaxResults@, Cloud Map returns up to 100 instances.
getInstancesHealthStatus_maxResults :: Lens.Lens' GetInstancesHealthStatus (Prelude.Maybe Prelude.Natural)
getInstancesHealthStatus_maxResults = Lens.lens (\GetInstancesHealthStatus' {maxResults} -> maxResults) (\s@GetInstancesHealthStatus' {} a -> s {maxResults = a} :: GetInstancesHealthStatus)

-- | For the first @GetInstancesHealthStatus@ request, omit this value.
--
-- If more than @MaxResults@ instances match the specified criteria, you
-- can submit another @GetInstancesHealthStatus@ request to get the next
-- group of results. Specify the value of @NextToken@ from the previous
-- response in the next request.
getInstancesHealthStatus_nextToken :: Lens.Lens' GetInstancesHealthStatus (Prelude.Maybe Prelude.Text)
getInstancesHealthStatus_nextToken = Lens.lens (\GetInstancesHealthStatus' {nextToken} -> nextToken) (\s@GetInstancesHealthStatus' {} a -> s {nextToken = a} :: GetInstancesHealthStatus)

-- | The ID of the service that the instance is associated with.
getInstancesHealthStatus_serviceId :: Lens.Lens' GetInstancesHealthStatus Prelude.Text
getInstancesHealthStatus_serviceId = Lens.lens (\GetInstancesHealthStatus' {serviceId} -> serviceId) (\s@GetInstancesHealthStatus' {} a -> s {serviceId = a} :: GetInstancesHealthStatus)

instance Core.AWSRequest GetInstancesHealthStatus where
  type
    AWSResponse GetInstancesHealthStatus =
      GetInstancesHealthStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstancesHealthStatusResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Status" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInstancesHealthStatus where
  hashWithSalt _salt GetInstancesHealthStatus' {..} =
    _salt `Prelude.hashWithSalt` instances
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` serviceId

instance Prelude.NFData GetInstancesHealthStatus where
  rnf GetInstancesHealthStatus' {..} =
    Prelude.rnf instances
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf serviceId

instance Data.ToHeaders GetInstancesHealthStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53AutoNaming_v20170314.GetInstancesHealthStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetInstancesHealthStatus where
  toJSON GetInstancesHealthStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Instances" Data..=) Prelude.<$> instances,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("ServiceId" Data..= serviceId)
          ]
      )

instance Data.ToPath GetInstancesHealthStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery GetInstancesHealthStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInstancesHealthStatusResponse' smart constructor.
data GetInstancesHealthStatusResponse = GetInstancesHealthStatusResponse'
  { -- | If more than @MaxResults@ instances match the specified criteria, you
    -- can submit another @GetInstancesHealthStatus@ request to get the next
    -- group of results. Specify the value of @NextToken@ from the previous
    -- response in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A complex type that contains the IDs and the health status of the
    -- instances that you specified in the @GetInstancesHealthStatus@ request.
    status :: Prelude.Maybe (Prelude.HashMap Prelude.Text HealthStatus),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetInstancesHealthStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getInstancesHealthStatusResponse_nextToken' - If more than @MaxResults@ instances match the specified criteria, you
-- can submit another @GetInstancesHealthStatus@ request to get the next
-- group of results. Specify the value of @NextToken@ from the previous
-- response in the next request.
--
-- 'status', 'getInstancesHealthStatusResponse_status' - A complex type that contains the IDs and the health status of the
-- instances that you specified in the @GetInstancesHealthStatus@ request.
--
-- 'httpStatus', 'getInstancesHealthStatusResponse_httpStatus' - The response's http status code.
newGetInstancesHealthStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInstancesHealthStatusResponse
newGetInstancesHealthStatusResponse pHttpStatus_ =
  GetInstancesHealthStatusResponse'
    { nextToken =
        Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If more than @MaxResults@ instances match the specified criteria, you
-- can submit another @GetInstancesHealthStatus@ request to get the next
-- group of results. Specify the value of @NextToken@ from the previous
-- response in the next request.
getInstancesHealthStatusResponse_nextToken :: Lens.Lens' GetInstancesHealthStatusResponse (Prelude.Maybe Prelude.Text)
getInstancesHealthStatusResponse_nextToken = Lens.lens (\GetInstancesHealthStatusResponse' {nextToken} -> nextToken) (\s@GetInstancesHealthStatusResponse' {} a -> s {nextToken = a} :: GetInstancesHealthStatusResponse)

-- | A complex type that contains the IDs and the health status of the
-- instances that you specified in the @GetInstancesHealthStatus@ request.
getInstancesHealthStatusResponse_status :: Lens.Lens' GetInstancesHealthStatusResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text HealthStatus))
getInstancesHealthStatusResponse_status = Lens.lens (\GetInstancesHealthStatusResponse' {status} -> status) (\s@GetInstancesHealthStatusResponse' {} a -> s {status = a} :: GetInstancesHealthStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getInstancesHealthStatusResponse_httpStatus :: Lens.Lens' GetInstancesHealthStatusResponse Prelude.Int
getInstancesHealthStatusResponse_httpStatus = Lens.lens (\GetInstancesHealthStatusResponse' {httpStatus} -> httpStatus) (\s@GetInstancesHealthStatusResponse' {} a -> s {httpStatus = a} :: GetInstancesHealthStatusResponse)

instance
  Prelude.NFData
    GetInstancesHealthStatusResponse
  where
  rnf GetInstancesHealthStatusResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
