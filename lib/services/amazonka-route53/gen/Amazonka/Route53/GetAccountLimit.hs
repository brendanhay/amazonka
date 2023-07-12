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
-- Module      : Amazonka.Route53.GetAccountLimit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified limit for the current account, for example, the
-- maximum number of health checks that you can create using the account.
--
-- For the default limit, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits>
-- in the /Amazon Route 53 Developer Guide/. To request a higher limit,
-- <https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&limitType=service-code-route53 open a case>.
--
-- You can also view account limits in Amazon Web Services Trusted Advisor.
-- Sign in to the Amazon Web Services Management Console and open the
-- Trusted Advisor console at
-- <https://console.aws.amazon.com/trustedadvisor https:\/\/console.aws.amazon.com\/trustedadvisor\/>.
-- Then choose __Service limits__ in the navigation pane.
module Amazonka.Route53.GetAccountLimit
  ( -- * Creating a Request
    GetAccountLimit (..),
    newGetAccountLimit,

    -- * Request Lenses
    getAccountLimit_type,

    -- * Destructuring the Response
    GetAccountLimitResponse (..),
    newGetAccountLimitResponse,

    -- * Response Lenses
    getAccountLimitResponse_httpStatus,
    getAccountLimitResponse_limit,
    getAccountLimitResponse_count,
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
-- /See:/ 'newGetAccountLimit' smart constructor.
data GetAccountLimit = GetAccountLimit'
  { -- | The limit that you want to get. Valid values include the following:
    --
    -- -   __MAX_HEALTH_CHECKS_BY_OWNER__: The maximum number of health checks
    --     that you can create using the current account.
    --
    -- -   __MAX_HOSTED_ZONES_BY_OWNER__: The maximum number of hosted zones
    --     that you can create using the current account.
    --
    -- -   __MAX_REUSABLE_DELEGATION_SETS_BY_OWNER__: The maximum number of
    --     reusable delegation sets that you can create using the current
    --     account.
    --
    -- -   __MAX_TRAFFIC_POLICIES_BY_OWNER__: The maximum number of traffic
    --     policies that you can create using the current account.
    --
    -- -   __MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER__: The maximum number of
    --     traffic policy instances that you can create using the current
    --     account. (Traffic policy instances are referred to as traffic flow
    --     policy records in the Amazon Route 53 console.)
    type' :: AccountLimitType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccountLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'getAccountLimit_type' - The limit that you want to get. Valid values include the following:
--
-- -   __MAX_HEALTH_CHECKS_BY_OWNER__: The maximum number of health checks
--     that you can create using the current account.
--
-- -   __MAX_HOSTED_ZONES_BY_OWNER__: The maximum number of hosted zones
--     that you can create using the current account.
--
-- -   __MAX_REUSABLE_DELEGATION_SETS_BY_OWNER__: The maximum number of
--     reusable delegation sets that you can create using the current
--     account.
--
-- -   __MAX_TRAFFIC_POLICIES_BY_OWNER__: The maximum number of traffic
--     policies that you can create using the current account.
--
-- -   __MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER__: The maximum number of
--     traffic policy instances that you can create using the current
--     account. (Traffic policy instances are referred to as traffic flow
--     policy records in the Amazon Route 53 console.)
newGetAccountLimit ::
  -- | 'type''
  AccountLimitType ->
  GetAccountLimit
newGetAccountLimit pType_ =
  GetAccountLimit' {type' = pType_}

-- | The limit that you want to get. Valid values include the following:
--
-- -   __MAX_HEALTH_CHECKS_BY_OWNER__: The maximum number of health checks
--     that you can create using the current account.
--
-- -   __MAX_HOSTED_ZONES_BY_OWNER__: The maximum number of hosted zones
--     that you can create using the current account.
--
-- -   __MAX_REUSABLE_DELEGATION_SETS_BY_OWNER__: The maximum number of
--     reusable delegation sets that you can create using the current
--     account.
--
-- -   __MAX_TRAFFIC_POLICIES_BY_OWNER__: The maximum number of traffic
--     policies that you can create using the current account.
--
-- -   __MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER__: The maximum number of
--     traffic policy instances that you can create using the current
--     account. (Traffic policy instances are referred to as traffic flow
--     policy records in the Amazon Route 53 console.)
getAccountLimit_type :: Lens.Lens' GetAccountLimit AccountLimitType
getAccountLimit_type = Lens.lens (\GetAccountLimit' {type'} -> type') (\s@GetAccountLimit' {} a -> s {type' = a} :: GetAccountLimit)

instance Core.AWSRequest GetAccountLimit where
  type
    AWSResponse GetAccountLimit =
      GetAccountLimitResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetAccountLimitResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "Limit")
            Prelude.<*> (x Data..@ "Count")
      )

instance Prelude.Hashable GetAccountLimit where
  hashWithSalt _salt GetAccountLimit' {..} =
    _salt `Prelude.hashWithSalt` type'

instance Prelude.NFData GetAccountLimit where
  rnf GetAccountLimit' {..} = Prelude.rnf type'

instance Data.ToHeaders GetAccountLimit where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetAccountLimit where
  toPath GetAccountLimit' {..} =
    Prelude.mconcat
      ["/2013-04-01/accountlimit/", Data.toBS type']

instance Data.ToQuery GetAccountLimit where
  toQuery = Prelude.const Prelude.mempty

-- | A complex type that contains the requested limit.
--
-- /See:/ 'newGetAccountLimitResponse' smart constructor.
data GetAccountLimitResponse = GetAccountLimitResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The current setting for the specified limit. For example, if you
    -- specified @MAX_HEALTH_CHECKS_BY_OWNER@ for the value of @Type@ in the
    -- request, the value of @Limit@ is the maximum number of health checks
    -- that you can create using the current account.
    limit :: AccountLimit,
    -- | The current number of entities that you have created of the specified
    -- type. For example, if you specified @MAX_HEALTH_CHECKS_BY_OWNER@ for the
    -- value of @Type@ in the request, the value of @Count@ is the current
    -- number of health checks that you have created using the current account.
    count :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccountLimitResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getAccountLimitResponse_httpStatus' - The response's http status code.
--
-- 'limit', 'getAccountLimitResponse_limit' - The current setting for the specified limit. For example, if you
-- specified @MAX_HEALTH_CHECKS_BY_OWNER@ for the value of @Type@ in the
-- request, the value of @Limit@ is the maximum number of health checks
-- that you can create using the current account.
--
-- 'count', 'getAccountLimitResponse_count' - The current number of entities that you have created of the specified
-- type. For example, if you specified @MAX_HEALTH_CHECKS_BY_OWNER@ for the
-- value of @Type@ in the request, the value of @Count@ is the current
-- number of health checks that you have created using the current account.
newGetAccountLimitResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'limit'
  AccountLimit ->
  -- | 'count'
  Prelude.Natural ->
  GetAccountLimitResponse
newGetAccountLimitResponse
  pHttpStatus_
  pLimit_
  pCount_ =
    GetAccountLimitResponse'
      { httpStatus = pHttpStatus_,
        limit = pLimit_,
        count = pCount_
      }

-- | The response's http status code.
getAccountLimitResponse_httpStatus :: Lens.Lens' GetAccountLimitResponse Prelude.Int
getAccountLimitResponse_httpStatus = Lens.lens (\GetAccountLimitResponse' {httpStatus} -> httpStatus) (\s@GetAccountLimitResponse' {} a -> s {httpStatus = a} :: GetAccountLimitResponse)

-- | The current setting for the specified limit. For example, if you
-- specified @MAX_HEALTH_CHECKS_BY_OWNER@ for the value of @Type@ in the
-- request, the value of @Limit@ is the maximum number of health checks
-- that you can create using the current account.
getAccountLimitResponse_limit :: Lens.Lens' GetAccountLimitResponse AccountLimit
getAccountLimitResponse_limit = Lens.lens (\GetAccountLimitResponse' {limit} -> limit) (\s@GetAccountLimitResponse' {} a -> s {limit = a} :: GetAccountLimitResponse)

-- | The current number of entities that you have created of the specified
-- type. For example, if you specified @MAX_HEALTH_CHECKS_BY_OWNER@ for the
-- value of @Type@ in the request, the value of @Count@ is the current
-- number of health checks that you have created using the current account.
getAccountLimitResponse_count :: Lens.Lens' GetAccountLimitResponse Prelude.Natural
getAccountLimitResponse_count = Lens.lens (\GetAccountLimitResponse' {count} -> count) (\s@GetAccountLimitResponse' {} a -> s {count = a} :: GetAccountLimitResponse)

instance Prelude.NFData GetAccountLimitResponse where
  rnf GetAccountLimitResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf count
