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
-- Module      : Amazonka.Route53.GetReusableDelegationSetLimit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the maximum number of hosted zones that you can associate with the
-- specified reusable delegation set.
--
-- For the default limit, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits>
-- in the /Amazon Route 53 Developer Guide/. To request a higher limit,
-- <https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&limitType=service-code-route53 open a case>.
module Amazonka.Route53.GetReusableDelegationSetLimit
  ( -- * Creating a Request
    GetReusableDelegationSetLimit (..),
    newGetReusableDelegationSetLimit,

    -- * Request Lenses
    getReusableDelegationSetLimit_type,
    getReusableDelegationSetLimit_delegationSetId,

    -- * Destructuring the Response
    GetReusableDelegationSetLimitResponse (..),
    newGetReusableDelegationSetLimitResponse,

    -- * Response Lenses
    getReusableDelegationSetLimitResponse_httpStatus,
    getReusableDelegationSetLimitResponse_limit,
    getReusableDelegationSetLimitResponse_count,
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
-- /See:/ 'newGetReusableDelegationSetLimit' smart constructor.
data GetReusableDelegationSetLimit = GetReusableDelegationSetLimit'
  { -- | Specify @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ to get the maximum number
    -- of hosted zones that you can associate with the specified reusable
    -- delegation set.
    type' :: ReusableDelegationSetLimitType,
    -- | The ID of the delegation set that you want to get the limit for.
    delegationSetId :: ResourceId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReusableDelegationSetLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'getReusableDelegationSetLimit_type' - Specify @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ to get the maximum number
-- of hosted zones that you can associate with the specified reusable
-- delegation set.
--
-- 'delegationSetId', 'getReusableDelegationSetLimit_delegationSetId' - The ID of the delegation set that you want to get the limit for.
newGetReusableDelegationSetLimit ::
  -- | 'type''
  ReusableDelegationSetLimitType ->
  -- | 'delegationSetId'
  ResourceId ->
  GetReusableDelegationSetLimit
newGetReusableDelegationSetLimit
  pType_
  pDelegationSetId_ =
    GetReusableDelegationSetLimit'
      { type' = pType_,
        delegationSetId = pDelegationSetId_
      }

-- | Specify @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ to get the maximum number
-- of hosted zones that you can associate with the specified reusable
-- delegation set.
getReusableDelegationSetLimit_type :: Lens.Lens' GetReusableDelegationSetLimit ReusableDelegationSetLimitType
getReusableDelegationSetLimit_type = Lens.lens (\GetReusableDelegationSetLimit' {type'} -> type') (\s@GetReusableDelegationSetLimit' {} a -> s {type' = a} :: GetReusableDelegationSetLimit)

-- | The ID of the delegation set that you want to get the limit for.
getReusableDelegationSetLimit_delegationSetId :: Lens.Lens' GetReusableDelegationSetLimit ResourceId
getReusableDelegationSetLimit_delegationSetId = Lens.lens (\GetReusableDelegationSetLimit' {delegationSetId} -> delegationSetId) (\s@GetReusableDelegationSetLimit' {} a -> s {delegationSetId = a} :: GetReusableDelegationSetLimit)

instance
  Core.AWSRequest
    GetReusableDelegationSetLimit
  where
  type
    AWSResponse GetReusableDelegationSetLimit =
      GetReusableDelegationSetLimitResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetReusableDelegationSetLimitResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "Limit")
            Prelude.<*> (x Data..@ "Count")
      )

instance
  Prelude.Hashable
    GetReusableDelegationSetLimit
  where
  hashWithSalt _salt GetReusableDelegationSetLimit' {..} =
    _salt
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` delegationSetId

instance Prelude.NFData GetReusableDelegationSetLimit where
  rnf GetReusableDelegationSetLimit' {..} =
    Prelude.rnf type' `Prelude.seq`
      Prelude.rnf delegationSetId

instance Data.ToHeaders GetReusableDelegationSetLimit where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetReusableDelegationSetLimit where
  toPath GetReusableDelegationSetLimit' {..} =
    Prelude.mconcat
      [ "/2013-04-01/reusabledelegationsetlimit/",
        Data.toBS delegationSetId,
        "/",
        Data.toBS type'
      ]

instance Data.ToQuery GetReusableDelegationSetLimit where
  toQuery = Prelude.const Prelude.mempty

-- | A complex type that contains the requested limit.
--
-- /See:/ 'newGetReusableDelegationSetLimitResponse' smart constructor.
data GetReusableDelegationSetLimitResponse = GetReusableDelegationSetLimitResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The current setting for the limit on hosted zones that you can associate
    -- with the specified reusable delegation set.
    limit :: ReusableDelegationSetLimit,
    -- | The current number of hosted zones that you can associate with the
    -- specified reusable delegation set.
    count :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReusableDelegationSetLimitResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getReusableDelegationSetLimitResponse_httpStatus' - The response's http status code.
--
-- 'limit', 'getReusableDelegationSetLimitResponse_limit' - The current setting for the limit on hosted zones that you can associate
-- with the specified reusable delegation set.
--
-- 'count', 'getReusableDelegationSetLimitResponse_count' - The current number of hosted zones that you can associate with the
-- specified reusable delegation set.
newGetReusableDelegationSetLimitResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'limit'
  ReusableDelegationSetLimit ->
  -- | 'count'
  Prelude.Natural ->
  GetReusableDelegationSetLimitResponse
newGetReusableDelegationSetLimitResponse
  pHttpStatus_
  pLimit_
  pCount_ =
    GetReusableDelegationSetLimitResponse'
      { httpStatus =
          pHttpStatus_,
        limit = pLimit_,
        count = pCount_
      }

-- | The response's http status code.
getReusableDelegationSetLimitResponse_httpStatus :: Lens.Lens' GetReusableDelegationSetLimitResponse Prelude.Int
getReusableDelegationSetLimitResponse_httpStatus = Lens.lens (\GetReusableDelegationSetLimitResponse' {httpStatus} -> httpStatus) (\s@GetReusableDelegationSetLimitResponse' {} a -> s {httpStatus = a} :: GetReusableDelegationSetLimitResponse)

-- | The current setting for the limit on hosted zones that you can associate
-- with the specified reusable delegation set.
getReusableDelegationSetLimitResponse_limit :: Lens.Lens' GetReusableDelegationSetLimitResponse ReusableDelegationSetLimit
getReusableDelegationSetLimitResponse_limit = Lens.lens (\GetReusableDelegationSetLimitResponse' {limit} -> limit) (\s@GetReusableDelegationSetLimitResponse' {} a -> s {limit = a} :: GetReusableDelegationSetLimitResponse)

-- | The current number of hosted zones that you can associate with the
-- specified reusable delegation set.
getReusableDelegationSetLimitResponse_count :: Lens.Lens' GetReusableDelegationSetLimitResponse Prelude.Natural
getReusableDelegationSetLimitResponse_count = Lens.lens (\GetReusableDelegationSetLimitResponse' {count} -> count) (\s@GetReusableDelegationSetLimitResponse' {} a -> s {count = a} :: GetReusableDelegationSetLimitResponse)

instance
  Prelude.NFData
    GetReusableDelegationSetLimitResponse
  where
  rnf GetReusableDelegationSetLimitResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf limit `Prelude.seq`
        Prelude.rnf count
