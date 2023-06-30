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
-- Module      : Amazonka.Route53.GetHostedZoneLimit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified limit for a specified hosted zone, for example, the
-- maximum number of records that you can create in the hosted zone.
--
-- For the default limit, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits>
-- in the /Amazon Route 53 Developer Guide/. To request a higher limit,
-- <https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&limitType=service-code-route53 open a case>.
module Amazonka.Route53.GetHostedZoneLimit
  ( -- * Creating a Request
    GetHostedZoneLimit (..),
    newGetHostedZoneLimit,

    -- * Request Lenses
    getHostedZoneLimit_type,
    getHostedZoneLimit_hostedZoneId,

    -- * Destructuring the Response
    GetHostedZoneLimitResponse (..),
    newGetHostedZoneLimitResponse,

    -- * Response Lenses
    getHostedZoneLimitResponse_httpStatus,
    getHostedZoneLimitResponse_limit,
    getHostedZoneLimitResponse_count,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | A complex type that contains information about the request to create a
-- hosted zone.
--
-- /See:/ 'newGetHostedZoneLimit' smart constructor.
data GetHostedZoneLimit = GetHostedZoneLimit'
  { -- | The limit that you want to get. Valid values include the following:
    --
    -- -   __MAX_RRSETS_BY_ZONE__: The maximum number of records that you can
    --     create in the specified hosted zone.
    --
    -- -   __MAX_VPCS_ASSOCIATED_BY_ZONE__: The maximum number of Amazon VPCs
    --     that you can associate with the specified private hosted zone.
    type' :: HostedZoneLimitType,
    -- | The ID of the hosted zone that you want to get a limit for.
    hostedZoneId :: ResourceId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHostedZoneLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'getHostedZoneLimit_type' - The limit that you want to get. Valid values include the following:
--
-- -   __MAX_RRSETS_BY_ZONE__: The maximum number of records that you can
--     create in the specified hosted zone.
--
-- -   __MAX_VPCS_ASSOCIATED_BY_ZONE__: The maximum number of Amazon VPCs
--     that you can associate with the specified private hosted zone.
--
-- 'hostedZoneId', 'getHostedZoneLimit_hostedZoneId' - The ID of the hosted zone that you want to get a limit for.
newGetHostedZoneLimit ::
  -- | 'type''
  HostedZoneLimitType ->
  -- | 'hostedZoneId'
  ResourceId ->
  GetHostedZoneLimit
newGetHostedZoneLimit pType_ pHostedZoneId_ =
  GetHostedZoneLimit'
    { type' = pType_,
      hostedZoneId = pHostedZoneId_
    }

-- | The limit that you want to get. Valid values include the following:
--
-- -   __MAX_RRSETS_BY_ZONE__: The maximum number of records that you can
--     create in the specified hosted zone.
--
-- -   __MAX_VPCS_ASSOCIATED_BY_ZONE__: The maximum number of Amazon VPCs
--     that you can associate with the specified private hosted zone.
getHostedZoneLimit_type :: Lens.Lens' GetHostedZoneLimit HostedZoneLimitType
getHostedZoneLimit_type = Lens.lens (\GetHostedZoneLimit' {type'} -> type') (\s@GetHostedZoneLimit' {} a -> s {type' = a} :: GetHostedZoneLimit)

-- | The ID of the hosted zone that you want to get a limit for.
getHostedZoneLimit_hostedZoneId :: Lens.Lens' GetHostedZoneLimit ResourceId
getHostedZoneLimit_hostedZoneId = Lens.lens (\GetHostedZoneLimit' {hostedZoneId} -> hostedZoneId) (\s@GetHostedZoneLimit' {} a -> s {hostedZoneId = a} :: GetHostedZoneLimit)

instance Core.AWSRequest GetHostedZoneLimit where
  type
    AWSResponse GetHostedZoneLimit =
      GetHostedZoneLimitResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetHostedZoneLimitResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "Limit")
            Prelude.<*> (x Data..@ "Count")
      )

instance Prelude.Hashable GetHostedZoneLimit where
  hashWithSalt _salt GetHostedZoneLimit' {..} =
    _salt
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` hostedZoneId

instance Prelude.NFData GetHostedZoneLimit where
  rnf GetHostedZoneLimit' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf hostedZoneId

instance Data.ToHeaders GetHostedZoneLimit where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetHostedZoneLimit where
  toPath GetHostedZoneLimit' {..} =
    Prelude.mconcat
      [ "/2013-04-01/hostedzonelimit/",
        Data.toBS hostedZoneId,
        "/",
        Data.toBS type'
      ]

instance Data.ToQuery GetHostedZoneLimit where
  toQuery = Prelude.const Prelude.mempty

-- | A complex type that contains the requested limit.
--
-- /See:/ 'newGetHostedZoneLimitResponse' smart constructor.
data GetHostedZoneLimitResponse = GetHostedZoneLimitResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The current setting for the specified limit. For example, if you
    -- specified @MAX_RRSETS_BY_ZONE@ for the value of @Type@ in the request,
    -- the value of @Limit@ is the maximum number of records that you can
    -- create in the specified hosted zone.
    limit :: HostedZoneLimit,
    -- | The current number of entities that you have created of the specified
    -- type. For example, if you specified @MAX_RRSETS_BY_ZONE@ for the value
    -- of @Type@ in the request, the value of @Count@ is the current number of
    -- records that you have created in the specified hosted zone.
    count :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHostedZoneLimitResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getHostedZoneLimitResponse_httpStatus' - The response's http status code.
--
-- 'limit', 'getHostedZoneLimitResponse_limit' - The current setting for the specified limit. For example, if you
-- specified @MAX_RRSETS_BY_ZONE@ for the value of @Type@ in the request,
-- the value of @Limit@ is the maximum number of records that you can
-- create in the specified hosted zone.
--
-- 'count', 'getHostedZoneLimitResponse_count' - The current number of entities that you have created of the specified
-- type. For example, if you specified @MAX_RRSETS_BY_ZONE@ for the value
-- of @Type@ in the request, the value of @Count@ is the current number of
-- records that you have created in the specified hosted zone.
newGetHostedZoneLimitResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'limit'
  HostedZoneLimit ->
  -- | 'count'
  Prelude.Natural ->
  GetHostedZoneLimitResponse
newGetHostedZoneLimitResponse
  pHttpStatus_
  pLimit_
  pCount_ =
    GetHostedZoneLimitResponse'
      { httpStatus =
          pHttpStatus_,
        limit = pLimit_,
        count = pCount_
      }

-- | The response's http status code.
getHostedZoneLimitResponse_httpStatus :: Lens.Lens' GetHostedZoneLimitResponse Prelude.Int
getHostedZoneLimitResponse_httpStatus = Lens.lens (\GetHostedZoneLimitResponse' {httpStatus} -> httpStatus) (\s@GetHostedZoneLimitResponse' {} a -> s {httpStatus = a} :: GetHostedZoneLimitResponse)

-- | The current setting for the specified limit. For example, if you
-- specified @MAX_RRSETS_BY_ZONE@ for the value of @Type@ in the request,
-- the value of @Limit@ is the maximum number of records that you can
-- create in the specified hosted zone.
getHostedZoneLimitResponse_limit :: Lens.Lens' GetHostedZoneLimitResponse HostedZoneLimit
getHostedZoneLimitResponse_limit = Lens.lens (\GetHostedZoneLimitResponse' {limit} -> limit) (\s@GetHostedZoneLimitResponse' {} a -> s {limit = a} :: GetHostedZoneLimitResponse)

-- | The current number of entities that you have created of the specified
-- type. For example, if you specified @MAX_RRSETS_BY_ZONE@ for the value
-- of @Type@ in the request, the value of @Count@ is the current number of
-- records that you have created in the specified hosted zone.
getHostedZoneLimitResponse_count :: Lens.Lens' GetHostedZoneLimitResponse Prelude.Natural
getHostedZoneLimitResponse_count = Lens.lens (\GetHostedZoneLimitResponse' {count} -> count) (\s@GetHostedZoneLimitResponse' {} a -> s {count = a} :: GetHostedZoneLimitResponse)

instance Prelude.NFData GetHostedZoneLimitResponse where
  rnf GetHostedZoneLimitResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf count
