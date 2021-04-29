{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetTrafficPolicyInstanceCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetTrafficPolicyInstanceCount ::
  GetTrafficPolicyInstanceCount
newGetTrafficPolicyInstanceCount =
  GetTrafficPolicyInstanceCount'

instance
  Prelude.AWSRequest
    GetTrafficPolicyInstanceCount
  where
  type
    Rs GetTrafficPolicyInstanceCount =
      GetTrafficPolicyInstanceCountResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetTrafficPolicyInstanceCountResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..@ "TrafficPolicyInstanceCount")
      )

instance
  Prelude.Hashable
    GetTrafficPolicyInstanceCount

instance Prelude.NFData GetTrafficPolicyInstanceCount

instance
  Prelude.ToHeaders
    GetTrafficPolicyInstanceCount
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetTrafficPolicyInstanceCount where
  toPath =
    Prelude.const
      "/2013-04-01/trafficpolicyinstancecount"

instance
  Prelude.ToQuery
    GetTrafficPolicyInstanceCount
  where
  toQuery = Prelude.const Prelude.mempty

-- | A complex type that contains information about the resource record sets
-- that Amazon Route 53 created based on a specified traffic policy.
--
-- /See:/ 'newGetTrafficPolicyInstanceCountResponse' smart constructor.
data GetTrafficPolicyInstanceCountResponse = GetTrafficPolicyInstanceCountResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The number of traffic policy instances that are associated with the
    -- current AWS account.
    trafficPolicyInstanceCount :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'trafficPolicyInstanceCount'
  Prelude.Int ->
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
getTrafficPolicyInstanceCountResponse_httpStatus :: Lens.Lens' GetTrafficPolicyInstanceCountResponse Prelude.Int
getTrafficPolicyInstanceCountResponse_httpStatus = Lens.lens (\GetTrafficPolicyInstanceCountResponse' {httpStatus} -> httpStatus) (\s@GetTrafficPolicyInstanceCountResponse' {} a -> s {httpStatus = a} :: GetTrafficPolicyInstanceCountResponse)

-- | The number of traffic policy instances that are associated with the
-- current AWS account.
getTrafficPolicyInstanceCountResponse_trafficPolicyInstanceCount :: Lens.Lens' GetTrafficPolicyInstanceCountResponse Prelude.Int
getTrafficPolicyInstanceCountResponse_trafficPolicyInstanceCount = Lens.lens (\GetTrafficPolicyInstanceCountResponse' {trafficPolicyInstanceCount} -> trafficPolicyInstanceCount) (\s@GetTrafficPolicyInstanceCountResponse' {} a -> s {trafficPolicyInstanceCount = a} :: GetTrafficPolicyInstanceCountResponse)

instance
  Prelude.NFData
    GetTrafficPolicyInstanceCountResponse
