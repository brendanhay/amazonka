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
-- Module      : Amazonka.Lightsail.GetDistributionLatestCacheReset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the timestamp and status of the last cache reset of a specific
-- Amazon Lightsail content delivery network (CDN) distribution.
module Amazonka.Lightsail.GetDistributionLatestCacheReset
  ( -- * Creating a Request
    GetDistributionLatestCacheReset (..),
    newGetDistributionLatestCacheReset,

    -- * Request Lenses
    getDistributionLatestCacheReset_distributionName,

    -- * Destructuring the Response
    GetDistributionLatestCacheResetResponse (..),
    newGetDistributionLatestCacheResetResponse,

    -- * Response Lenses
    getDistributionLatestCacheResetResponse_createTime,
    getDistributionLatestCacheResetResponse_status,
    getDistributionLatestCacheResetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDistributionLatestCacheReset' smart constructor.
data GetDistributionLatestCacheReset = GetDistributionLatestCacheReset'
  { -- | The name of the distribution for which to return the timestamp of the
    -- last cache reset.
    --
    -- Use the @GetDistributions@ action to get a list of distribution names
    -- that you can specify.
    --
    -- When omitted, the response includes the latest cache reset timestamp of
    -- all your distributions.
    distributionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDistributionLatestCacheReset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionName', 'getDistributionLatestCacheReset_distributionName' - The name of the distribution for which to return the timestamp of the
-- last cache reset.
--
-- Use the @GetDistributions@ action to get a list of distribution names
-- that you can specify.
--
-- When omitted, the response includes the latest cache reset timestamp of
-- all your distributions.
newGetDistributionLatestCacheReset ::
  GetDistributionLatestCacheReset
newGetDistributionLatestCacheReset =
  GetDistributionLatestCacheReset'
    { distributionName =
        Prelude.Nothing
    }

-- | The name of the distribution for which to return the timestamp of the
-- last cache reset.
--
-- Use the @GetDistributions@ action to get a list of distribution names
-- that you can specify.
--
-- When omitted, the response includes the latest cache reset timestamp of
-- all your distributions.
getDistributionLatestCacheReset_distributionName :: Lens.Lens' GetDistributionLatestCacheReset (Prelude.Maybe Prelude.Text)
getDistributionLatestCacheReset_distributionName = Lens.lens (\GetDistributionLatestCacheReset' {distributionName} -> distributionName) (\s@GetDistributionLatestCacheReset' {} a -> s {distributionName = a} :: GetDistributionLatestCacheReset)

instance
  Core.AWSRequest
    GetDistributionLatestCacheReset
  where
  type
    AWSResponse GetDistributionLatestCacheReset =
      GetDistributionLatestCacheResetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDistributionLatestCacheResetResponse'
            Prelude.<$> (x Data..?> "createTime")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetDistributionLatestCacheReset
  where
  hashWithSalt
    _salt
    GetDistributionLatestCacheReset' {..} =
      _salt `Prelude.hashWithSalt` distributionName

instance
  Prelude.NFData
    GetDistributionLatestCacheReset
  where
  rnf GetDistributionLatestCacheReset' {..} =
    Prelude.rnf distributionName

instance
  Data.ToHeaders
    GetDistributionLatestCacheReset
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetDistributionLatestCacheReset" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDistributionLatestCacheReset where
  toJSON GetDistributionLatestCacheReset' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("distributionName" Data..=)
              Prelude.<$> distributionName
          ]
      )

instance Data.ToPath GetDistributionLatestCacheReset where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDistributionLatestCacheReset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDistributionLatestCacheResetResponse' smart constructor.
data GetDistributionLatestCacheResetResponse = GetDistributionLatestCacheResetResponse'
  { -- | The timestamp of the last cache reset (e.g., @1479734909.17@) in Unix
    -- time format.
    createTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the last cache reset.
    status :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDistributionLatestCacheResetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createTime', 'getDistributionLatestCacheResetResponse_createTime' - The timestamp of the last cache reset (e.g., @1479734909.17@) in Unix
-- time format.
--
-- 'status', 'getDistributionLatestCacheResetResponse_status' - The status of the last cache reset.
--
-- 'httpStatus', 'getDistributionLatestCacheResetResponse_httpStatus' - The response's http status code.
newGetDistributionLatestCacheResetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDistributionLatestCacheResetResponse
newGetDistributionLatestCacheResetResponse
  pHttpStatus_ =
    GetDistributionLatestCacheResetResponse'
      { createTime =
          Prelude.Nothing,
        status = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The timestamp of the last cache reset (e.g., @1479734909.17@) in Unix
-- time format.
getDistributionLatestCacheResetResponse_createTime :: Lens.Lens' GetDistributionLatestCacheResetResponse (Prelude.Maybe Prelude.UTCTime)
getDistributionLatestCacheResetResponse_createTime = Lens.lens (\GetDistributionLatestCacheResetResponse' {createTime} -> createTime) (\s@GetDistributionLatestCacheResetResponse' {} a -> s {createTime = a} :: GetDistributionLatestCacheResetResponse) Prelude.. Lens.mapping Data._Time

-- | The status of the last cache reset.
getDistributionLatestCacheResetResponse_status :: Lens.Lens' GetDistributionLatestCacheResetResponse (Prelude.Maybe Prelude.Text)
getDistributionLatestCacheResetResponse_status = Lens.lens (\GetDistributionLatestCacheResetResponse' {status} -> status) (\s@GetDistributionLatestCacheResetResponse' {} a -> s {status = a} :: GetDistributionLatestCacheResetResponse)

-- | The response's http status code.
getDistributionLatestCacheResetResponse_httpStatus :: Lens.Lens' GetDistributionLatestCacheResetResponse Prelude.Int
getDistributionLatestCacheResetResponse_httpStatus = Lens.lens (\GetDistributionLatestCacheResetResponse' {httpStatus} -> httpStatus) (\s@GetDistributionLatestCacheResetResponse' {} a -> s {httpStatus = a} :: GetDistributionLatestCacheResetResponse)

instance
  Prelude.NFData
    GetDistributionLatestCacheResetResponse
  where
  rnf GetDistributionLatestCacheResetResponse' {..} =
    Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
