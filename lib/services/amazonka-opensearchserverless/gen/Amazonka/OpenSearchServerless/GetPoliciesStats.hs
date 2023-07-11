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
-- Module      : Amazonka.OpenSearchServerless.GetPoliciesStats
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns statistical information about your OpenSearch Serverless access
-- policies, security configurations, and security policies.
module Amazonka.OpenSearchServerless.GetPoliciesStats
  ( -- * Creating a Request
    GetPoliciesStats (..),
    newGetPoliciesStats,

    -- * Destructuring the Response
    GetPoliciesStatsResponse (..),
    newGetPoliciesStatsResponse,

    -- * Response Lenses
    getPoliciesStatsResponse_accessPolicyStats,
    getPoliciesStatsResponse_securityConfigStats,
    getPoliciesStatsResponse_securityPolicyStats,
    getPoliciesStatsResponse_totalPolicyCount,
    getPoliciesStatsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPoliciesStats' smart constructor.
data GetPoliciesStats = GetPoliciesStats'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPoliciesStats' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetPoliciesStats ::
  GetPoliciesStats
newGetPoliciesStats = GetPoliciesStats'

instance Core.AWSRequest GetPoliciesStats where
  type
    AWSResponse GetPoliciesStats =
      GetPoliciesStatsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPoliciesStatsResponse'
            Prelude.<$> (x Data..?> "AccessPolicyStats")
            Prelude.<*> (x Data..?> "SecurityConfigStats")
            Prelude.<*> (x Data..?> "SecurityPolicyStats")
            Prelude.<*> (x Data..?> "TotalPolicyCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPoliciesStats where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetPoliciesStats where
  rnf _ = ()

instance Data.ToHeaders GetPoliciesStats where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.GetPoliciesStats" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetPoliciesStats where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetPoliciesStats where
  toPath = Prelude.const "/"

instance Data.ToQuery GetPoliciesStats where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPoliciesStatsResponse' smart constructor.
data GetPoliciesStatsResponse = GetPoliciesStatsResponse'
  { -- | Information about the data access policies in your account.
    accessPolicyStats :: Prelude.Maybe AccessPolicyStats,
    -- | Information about the security configurations in your account.
    securityConfigStats :: Prelude.Maybe SecurityConfigStats,
    -- | Information about the security policies in your account.
    securityPolicyStats :: Prelude.Maybe SecurityPolicyStats,
    -- | The total number of OpenSearch Serverless security policies and
    -- configurations in your account.
    totalPolicyCount :: Prelude.Maybe Prelude.Integer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPoliciesStatsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessPolicyStats', 'getPoliciesStatsResponse_accessPolicyStats' - Information about the data access policies in your account.
--
-- 'securityConfigStats', 'getPoliciesStatsResponse_securityConfigStats' - Information about the security configurations in your account.
--
-- 'securityPolicyStats', 'getPoliciesStatsResponse_securityPolicyStats' - Information about the security policies in your account.
--
-- 'totalPolicyCount', 'getPoliciesStatsResponse_totalPolicyCount' - The total number of OpenSearch Serverless security policies and
-- configurations in your account.
--
-- 'httpStatus', 'getPoliciesStatsResponse_httpStatus' - The response's http status code.
newGetPoliciesStatsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPoliciesStatsResponse
newGetPoliciesStatsResponse pHttpStatus_ =
  GetPoliciesStatsResponse'
    { accessPolicyStats =
        Prelude.Nothing,
      securityConfigStats = Prelude.Nothing,
      securityPolicyStats = Prelude.Nothing,
      totalPolicyCount = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the data access policies in your account.
getPoliciesStatsResponse_accessPolicyStats :: Lens.Lens' GetPoliciesStatsResponse (Prelude.Maybe AccessPolicyStats)
getPoliciesStatsResponse_accessPolicyStats = Lens.lens (\GetPoliciesStatsResponse' {accessPolicyStats} -> accessPolicyStats) (\s@GetPoliciesStatsResponse' {} a -> s {accessPolicyStats = a} :: GetPoliciesStatsResponse)

-- | Information about the security configurations in your account.
getPoliciesStatsResponse_securityConfigStats :: Lens.Lens' GetPoliciesStatsResponse (Prelude.Maybe SecurityConfigStats)
getPoliciesStatsResponse_securityConfigStats = Lens.lens (\GetPoliciesStatsResponse' {securityConfigStats} -> securityConfigStats) (\s@GetPoliciesStatsResponse' {} a -> s {securityConfigStats = a} :: GetPoliciesStatsResponse)

-- | Information about the security policies in your account.
getPoliciesStatsResponse_securityPolicyStats :: Lens.Lens' GetPoliciesStatsResponse (Prelude.Maybe SecurityPolicyStats)
getPoliciesStatsResponse_securityPolicyStats = Lens.lens (\GetPoliciesStatsResponse' {securityPolicyStats} -> securityPolicyStats) (\s@GetPoliciesStatsResponse' {} a -> s {securityPolicyStats = a} :: GetPoliciesStatsResponse)

-- | The total number of OpenSearch Serverless security policies and
-- configurations in your account.
getPoliciesStatsResponse_totalPolicyCount :: Lens.Lens' GetPoliciesStatsResponse (Prelude.Maybe Prelude.Integer)
getPoliciesStatsResponse_totalPolicyCount = Lens.lens (\GetPoliciesStatsResponse' {totalPolicyCount} -> totalPolicyCount) (\s@GetPoliciesStatsResponse' {} a -> s {totalPolicyCount = a} :: GetPoliciesStatsResponse)

-- | The response's http status code.
getPoliciesStatsResponse_httpStatus :: Lens.Lens' GetPoliciesStatsResponse Prelude.Int
getPoliciesStatsResponse_httpStatus = Lens.lens (\GetPoliciesStatsResponse' {httpStatus} -> httpStatus) (\s@GetPoliciesStatsResponse' {} a -> s {httpStatus = a} :: GetPoliciesStatsResponse)

instance Prelude.NFData GetPoliciesStatsResponse where
  rnf GetPoliciesStatsResponse' {..} =
    Prelude.rnf accessPolicyStats
      `Prelude.seq` Prelude.rnf securityConfigStats
      `Prelude.seq` Prelude.rnf securityPolicyStats
      `Prelude.seq` Prelude.rnf totalPolicyCount
      `Prelude.seq` Prelude.rnf httpStatus
