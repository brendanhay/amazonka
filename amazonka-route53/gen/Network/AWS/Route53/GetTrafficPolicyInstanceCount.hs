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
-- Module      : Network.AWS.Route53.GetTrafficPolicyInstanceCount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the number of traffic policy instances that are associated with the
-- current AWS account.
module Network.AWS.Route53.GetTrafficPolicyInstanceCount
  ( -- * Creating a Request
    GetTrafficPolicyInstanceCount (..),
    newGetTrafficPolicyInstanceCount,

    -- * Destructuring the Response
    GetTrafficPolicyInstanceCountResponse (..),
    newGetTrafficPolicyInstanceCountResponse,

    -- * Response Lenses
    getTrafficPolicyInstanceCountResponse_httpStatus,
    getTrafficPolicyInstanceCountResponse_trafficPolicyInstanceCount,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | Request to get the number of traffic policy instances that are
-- associated with the current AWS account.
--
-- /See:/ 'newGetTrafficPolicyInstanceCount' smart constructor.
data GetTrafficPolicyInstanceCount = GetTrafficPolicyInstanceCount'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTrafficPolicyInstanceCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetTrafficPolicyInstanceCount ::
  GetTrafficPolicyInstanceCount
newGetTrafficPolicyInstanceCount =
  GetTrafficPolicyInstanceCount'

instance
  Core.AWSRequest
    GetTrafficPolicyInstanceCount
  where
  type
    AWSResponse GetTrafficPolicyInstanceCount =
      GetTrafficPolicyInstanceCountResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetTrafficPolicyInstanceCountResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..@ "TrafficPolicyInstanceCount")
      )

instance Core.Hashable GetTrafficPolicyInstanceCount

instance Core.NFData GetTrafficPolicyInstanceCount

instance Core.ToHeaders GetTrafficPolicyInstanceCount where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetTrafficPolicyInstanceCount where
  toPath =
    Core.const "/2013-04-01/trafficpolicyinstancecount"

instance Core.ToQuery GetTrafficPolicyInstanceCount where
  toQuery = Core.const Core.mempty

-- | A complex type that contains information about the resource record sets
-- that Amazon Route 53 created based on a specified traffic policy.
--
-- /See:/ 'newGetTrafficPolicyInstanceCountResponse' smart constructor.
data GetTrafficPolicyInstanceCountResponse = GetTrafficPolicyInstanceCountResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The number of traffic policy instances that are associated with the
    -- current AWS account.
    trafficPolicyInstanceCount :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTrafficPolicyInstanceCountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getTrafficPolicyInstanceCountResponse_httpStatus' - The response's http status code.
--
-- 'trafficPolicyInstanceCount', 'getTrafficPolicyInstanceCountResponse_trafficPolicyInstanceCount' - The number of traffic policy instances that are associated with the
-- current AWS account.
newGetTrafficPolicyInstanceCountResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'trafficPolicyInstanceCount'
  Core.Int ->
  GetTrafficPolicyInstanceCountResponse
newGetTrafficPolicyInstanceCountResponse
  pHttpStatus_
  pTrafficPolicyInstanceCount_ =
    GetTrafficPolicyInstanceCountResponse'
      { httpStatus =
          pHttpStatus_,
        trafficPolicyInstanceCount =
          pTrafficPolicyInstanceCount_
      }

-- | The response's http status code.
getTrafficPolicyInstanceCountResponse_httpStatus :: Lens.Lens' GetTrafficPolicyInstanceCountResponse Core.Int
getTrafficPolicyInstanceCountResponse_httpStatus = Lens.lens (\GetTrafficPolicyInstanceCountResponse' {httpStatus} -> httpStatus) (\s@GetTrafficPolicyInstanceCountResponse' {} a -> s {httpStatus = a} :: GetTrafficPolicyInstanceCountResponse)

-- | The number of traffic policy instances that are associated with the
-- current AWS account.
getTrafficPolicyInstanceCountResponse_trafficPolicyInstanceCount :: Lens.Lens' GetTrafficPolicyInstanceCountResponse Core.Int
getTrafficPolicyInstanceCountResponse_trafficPolicyInstanceCount = Lens.lens (\GetTrafficPolicyInstanceCountResponse' {trafficPolicyInstanceCount} -> trafficPolicyInstanceCount) (\s@GetTrafficPolicyInstanceCountResponse' {} a -> s {trafficPolicyInstanceCount = a} :: GetTrafficPolicyInstanceCountResponse)

instance
  Core.NFData
    GetTrafficPolicyInstanceCountResponse
