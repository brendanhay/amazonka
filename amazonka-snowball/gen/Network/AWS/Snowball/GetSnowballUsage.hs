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
-- Module      : Network.AWS.Snowball.GetSnowballUsage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the Snow Family service limit for your
-- account, and also the number of Snow devices your account has in use.
--
-- The default service limit for the number of Snow devices that you can
-- have at one time is 1. If you want to increase your service limit,
-- contact AWS Support.
module Network.AWS.Snowball.GetSnowballUsage
  ( -- * Creating a Request
    GetSnowballUsage (..),
    newGetSnowballUsage,

    -- * Destructuring the Response
    GetSnowballUsageResponse (..),
    newGetSnowballUsageResponse,

    -- * Response Lenses
    getSnowballUsageResponse_snowballsInUse,
    getSnowballUsageResponse_snowballLimit,
    getSnowballUsageResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Snowball.Types

-- | /See:/ 'newGetSnowballUsage' smart constructor.
data GetSnowballUsage = GetSnowballUsage'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetSnowballUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetSnowballUsage ::
  GetSnowballUsage
newGetSnowballUsage = GetSnowballUsage'

instance Prelude.AWSRequest GetSnowballUsage where
  type Rs GetSnowballUsage = GetSnowballUsageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSnowballUsageResponse'
            Prelude.<$> (x Prelude..?> "SnowballsInUse")
            Prelude.<*> (x Prelude..?> "SnowballLimit")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSnowballUsage

instance Prelude.NFData GetSnowballUsage

instance Prelude.ToHeaders GetSnowballUsage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSIESnowballJobManagementService.GetSnowballUsage" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetSnowballUsage where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath GetSnowballUsage where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetSnowballUsage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSnowballUsageResponse' smart constructor.
data GetSnowballUsageResponse = GetSnowballUsageResponse'
  { -- | The number of Snow devices that this account is currently using.
    snowballsInUse :: Prelude.Maybe Prelude.Int,
    -- | The service limit for number of Snow devices this account can have at
    -- once. The default service limit is 1 (one).
    snowballLimit :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetSnowballUsageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snowballsInUse', 'getSnowballUsageResponse_snowballsInUse' - The number of Snow devices that this account is currently using.
--
-- 'snowballLimit', 'getSnowballUsageResponse_snowballLimit' - The service limit for number of Snow devices this account can have at
-- once. The default service limit is 1 (one).
--
-- 'httpStatus', 'getSnowballUsageResponse_httpStatus' - The response's http status code.
newGetSnowballUsageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSnowballUsageResponse
newGetSnowballUsageResponse pHttpStatus_ =
  GetSnowballUsageResponse'
    { snowballsInUse =
        Prelude.Nothing,
      snowballLimit = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of Snow devices that this account is currently using.
getSnowballUsageResponse_snowballsInUse :: Lens.Lens' GetSnowballUsageResponse (Prelude.Maybe Prelude.Int)
getSnowballUsageResponse_snowballsInUse = Lens.lens (\GetSnowballUsageResponse' {snowballsInUse} -> snowballsInUse) (\s@GetSnowballUsageResponse' {} a -> s {snowballsInUse = a} :: GetSnowballUsageResponse)

-- | The service limit for number of Snow devices this account can have at
-- once. The default service limit is 1 (one).
getSnowballUsageResponse_snowballLimit :: Lens.Lens' GetSnowballUsageResponse (Prelude.Maybe Prelude.Int)
getSnowballUsageResponse_snowballLimit = Lens.lens (\GetSnowballUsageResponse' {snowballLimit} -> snowballLimit) (\s@GetSnowballUsageResponse' {} a -> s {snowballLimit = a} :: GetSnowballUsageResponse)

-- | The response's http status code.
getSnowballUsageResponse_httpStatus :: Lens.Lens' GetSnowballUsageResponse Prelude.Int
getSnowballUsageResponse_httpStatus = Lens.lens (\GetSnowballUsageResponse' {httpStatus} -> httpStatus) (\s@GetSnowballUsageResponse' {} a -> s {httpStatus = a} :: GetSnowballUsageResponse)

instance Prelude.NFData GetSnowballUsageResponse
