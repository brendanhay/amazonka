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
-- Module      : Amazonka.Snowball.GetSnowballUsage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the Snow Family service limit for your
-- account, and also the number of Snow devices your account has in use.
--
-- The default service limit for the number of Snow devices that you can
-- have at one time is 1. If you want to increase your service limit,
-- contact Amazon Web Services Support.
module Amazonka.Snowball.GetSnowballUsage
  ( -- * Creating a Request
    GetSnowballUsage (..),
    newGetSnowballUsage,

    -- * Destructuring the Response
    GetSnowballUsageResponse (..),
    newGetSnowballUsageResponse,

    -- * Response Lenses
    getSnowballUsageResponse_snowballLimit,
    getSnowballUsageResponse_snowballsInUse,
    getSnowballUsageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Snowball.Types

-- | /See:/ 'newGetSnowballUsage' smart constructor.
data GetSnowballUsage = GetSnowballUsage'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSnowballUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetSnowballUsage ::
  GetSnowballUsage
newGetSnowballUsage = GetSnowballUsage'

instance Core.AWSRequest GetSnowballUsage where
  type
    AWSResponse GetSnowballUsage =
      GetSnowballUsageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSnowballUsageResponse'
            Prelude.<$> (x Data..?> "SnowballLimit")
            Prelude.<*> (x Data..?> "SnowballsInUse")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSnowballUsage where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetSnowballUsage where
  rnf _ = ()

instance Data.ToHeaders GetSnowballUsage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIESnowballJobManagementService.GetSnowballUsage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSnowballUsage where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetSnowballUsage where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSnowballUsage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSnowballUsageResponse' smart constructor.
data GetSnowballUsageResponse = GetSnowballUsageResponse'
  { -- | The service limit for number of Snow devices this account can have at
    -- once. The default service limit is 1 (one).
    snowballLimit :: Prelude.Maybe Prelude.Int,
    -- | The number of Snow devices that this account is currently using.
    snowballsInUse :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSnowballUsageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snowballLimit', 'getSnowballUsageResponse_snowballLimit' - The service limit for number of Snow devices this account can have at
-- once. The default service limit is 1 (one).
--
-- 'snowballsInUse', 'getSnowballUsageResponse_snowballsInUse' - The number of Snow devices that this account is currently using.
--
-- 'httpStatus', 'getSnowballUsageResponse_httpStatus' - The response's http status code.
newGetSnowballUsageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSnowballUsageResponse
newGetSnowballUsageResponse pHttpStatus_ =
  GetSnowballUsageResponse'
    { snowballLimit =
        Prelude.Nothing,
      snowballsInUse = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The service limit for number of Snow devices this account can have at
-- once. The default service limit is 1 (one).
getSnowballUsageResponse_snowballLimit :: Lens.Lens' GetSnowballUsageResponse (Prelude.Maybe Prelude.Int)
getSnowballUsageResponse_snowballLimit = Lens.lens (\GetSnowballUsageResponse' {snowballLimit} -> snowballLimit) (\s@GetSnowballUsageResponse' {} a -> s {snowballLimit = a} :: GetSnowballUsageResponse)

-- | The number of Snow devices that this account is currently using.
getSnowballUsageResponse_snowballsInUse :: Lens.Lens' GetSnowballUsageResponse (Prelude.Maybe Prelude.Int)
getSnowballUsageResponse_snowballsInUse = Lens.lens (\GetSnowballUsageResponse' {snowballsInUse} -> snowballsInUse) (\s@GetSnowballUsageResponse' {} a -> s {snowballsInUse = a} :: GetSnowballUsageResponse)

-- | The response's http status code.
getSnowballUsageResponse_httpStatus :: Lens.Lens' GetSnowballUsageResponse Prelude.Int
getSnowballUsageResponse_httpStatus = Lens.lens (\GetSnowballUsageResponse' {httpStatus} -> httpStatus) (\s@GetSnowballUsageResponse' {} a -> s {httpStatus = a} :: GetSnowballUsageResponse)

instance Prelude.NFData GetSnowballUsageResponse where
  rnf GetSnowballUsageResponse' {..} =
    Prelude.rnf snowballLimit `Prelude.seq`
      Prelude.rnf snowballsInUse `Prelude.seq`
        Prelude.rnf httpStatus
