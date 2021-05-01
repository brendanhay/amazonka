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
-- Module      : Network.AWS.Lightsail.GetDistributionLatestCacheReset
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the timestamp and status of the last cache reset of a specific
-- Amazon Lightsail content delivery network (CDN) distribution.
module Network.AWS.Lightsail.GetDistributionLatestCacheReset
  ( -- * Creating a Request
    GetDistributionLatestCacheReset (..),
    newGetDistributionLatestCacheReset,

    -- * Request Lenses
    getDistributionLatestCacheReset_distributionName,

    -- * Destructuring the Response
    GetDistributionLatestCacheResetResponse (..),
    newGetDistributionLatestCacheResetResponse,

    -- * Response Lenses
    getDistributionLatestCacheResetResponse_status,
    getDistributionLatestCacheResetResponse_createTime,
    getDistributionLatestCacheResetResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    GetDistributionLatestCacheReset
  where
  type
    Rs GetDistributionLatestCacheReset =
      GetDistributionLatestCacheResetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDistributionLatestCacheResetResponse'
            Prelude.<$> (x Prelude..?> "status")
            Prelude.<*> (x Prelude..?> "createTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetDistributionLatestCacheReset

instance
  Prelude.NFData
    GetDistributionLatestCacheReset

instance
  Prelude.ToHeaders
    GetDistributionLatestCacheReset
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Lightsail_20161128.GetDistributionLatestCacheReset" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    GetDistributionLatestCacheReset
  where
  toJSON GetDistributionLatestCacheReset' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("distributionName" Prelude..=)
              Prelude.<$> distributionName
          ]
      )

instance
  Prelude.ToPath
    GetDistributionLatestCacheReset
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    GetDistributionLatestCacheReset
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDistributionLatestCacheResetResponse' smart constructor.
data GetDistributionLatestCacheResetResponse = GetDistributionLatestCacheResetResponse'
  { -- | The status of the last cache reset.
    status :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the last cache reset (e.g., @1479734909.17@) in Unix
    -- time format.
    createTime :: Prelude.Maybe Prelude.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetDistributionLatestCacheResetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getDistributionLatestCacheResetResponse_status' - The status of the last cache reset.
--
-- 'createTime', 'getDistributionLatestCacheResetResponse_createTime' - The timestamp of the last cache reset (e.g., @1479734909.17@) in Unix
-- time format.
--
-- 'httpStatus', 'getDistributionLatestCacheResetResponse_httpStatus' - The response's http status code.
newGetDistributionLatestCacheResetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDistributionLatestCacheResetResponse
newGetDistributionLatestCacheResetResponse
  pHttpStatus_ =
    GetDistributionLatestCacheResetResponse'
      { status =
          Prelude.Nothing,
        createTime = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The status of the last cache reset.
getDistributionLatestCacheResetResponse_status :: Lens.Lens' GetDistributionLatestCacheResetResponse (Prelude.Maybe Prelude.Text)
getDistributionLatestCacheResetResponse_status = Lens.lens (\GetDistributionLatestCacheResetResponse' {status} -> status) (\s@GetDistributionLatestCacheResetResponse' {} a -> s {status = a} :: GetDistributionLatestCacheResetResponse)

-- | The timestamp of the last cache reset (e.g., @1479734909.17@) in Unix
-- time format.
getDistributionLatestCacheResetResponse_createTime :: Lens.Lens' GetDistributionLatestCacheResetResponse (Prelude.Maybe Prelude.UTCTime)
getDistributionLatestCacheResetResponse_createTime = Lens.lens (\GetDistributionLatestCacheResetResponse' {createTime} -> createTime) (\s@GetDistributionLatestCacheResetResponse' {} a -> s {createTime = a} :: GetDistributionLatestCacheResetResponse) Prelude.. Lens.mapping Prelude._Time

-- | The response's http status code.
getDistributionLatestCacheResetResponse_httpStatus :: Lens.Lens' GetDistributionLatestCacheResetResponse Prelude.Int
getDistributionLatestCacheResetResponse_httpStatus = Lens.lens (\GetDistributionLatestCacheResetResponse' {httpStatus} -> httpStatus) (\s@GetDistributionLatestCacheResetResponse' {} a -> s {httpStatus = a} :: GetDistributionLatestCacheResetResponse)

instance
  Prelude.NFData
    GetDistributionLatestCacheResetResponse
