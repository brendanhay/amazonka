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
-- Module      : Network.AWS.Lambda.GetAccountSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details about your account\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/limits.html limits> and
-- usage in an AWS Region.
module Network.AWS.Lambda.GetAccountSettings
  ( -- * Creating a Request
    GetAccountSettings (..),
    newGetAccountSettings,

    -- * Destructuring the Response
    GetAccountSettingsResponse (..),
    newGetAccountSettingsResponse,

    -- * Response Lenses
    getAccountSettingsResponse_accountLimit,
    getAccountSettingsResponse_accountUsage,
    getAccountSettingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAccountSettings' smart constructor.
data GetAccountSettings = GetAccountSettings'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAccountSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetAccountSettings ::
  GetAccountSettings
newGetAccountSettings = GetAccountSettings'

instance Core.AWSRequest GetAccountSettings where
  type
    AWSResponse GetAccountSettings =
      GetAccountSettingsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAccountSettingsResponse'
            Core.<$> (x Core..?> "AccountLimit")
            Core.<*> (x Core..?> "AccountUsage")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetAccountSettings

instance Core.NFData GetAccountSettings

instance Core.ToHeaders GetAccountSettings where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetAccountSettings where
  toPath = Core.const "/2016-08-19/account-settings/"

instance Core.ToQuery GetAccountSettings where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetAccountSettingsResponse' smart constructor.
data GetAccountSettingsResponse = GetAccountSettingsResponse'
  { -- | Limits that are related to concurrency and code storage.
    accountLimit :: Core.Maybe AccountLimit,
    -- | The number of functions and amount of storage in use.
    accountUsage :: Core.Maybe AccountUsage,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAccountSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountLimit', 'getAccountSettingsResponse_accountLimit' - Limits that are related to concurrency and code storage.
--
-- 'accountUsage', 'getAccountSettingsResponse_accountUsage' - The number of functions and amount of storage in use.
--
-- 'httpStatus', 'getAccountSettingsResponse_httpStatus' - The response's http status code.
newGetAccountSettingsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetAccountSettingsResponse
newGetAccountSettingsResponse pHttpStatus_ =
  GetAccountSettingsResponse'
    { accountLimit =
        Core.Nothing,
      accountUsage = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Limits that are related to concurrency and code storage.
getAccountSettingsResponse_accountLimit :: Lens.Lens' GetAccountSettingsResponse (Core.Maybe AccountLimit)
getAccountSettingsResponse_accountLimit = Lens.lens (\GetAccountSettingsResponse' {accountLimit} -> accountLimit) (\s@GetAccountSettingsResponse' {} a -> s {accountLimit = a} :: GetAccountSettingsResponse)

-- | The number of functions and amount of storage in use.
getAccountSettingsResponse_accountUsage :: Lens.Lens' GetAccountSettingsResponse (Core.Maybe AccountUsage)
getAccountSettingsResponse_accountUsage = Lens.lens (\GetAccountSettingsResponse' {accountUsage} -> accountUsage) (\s@GetAccountSettingsResponse' {} a -> s {accountUsage = a} :: GetAccountSettingsResponse)

-- | The response's http status code.
getAccountSettingsResponse_httpStatus :: Lens.Lens' GetAccountSettingsResponse Core.Int
getAccountSettingsResponse_httpStatus = Lens.lens (\GetAccountSettingsResponse' {httpStatus} -> httpStatus) (\s@GetAccountSettingsResponse' {} a -> s {httpStatus = a} :: GetAccountSettingsResponse)

instance Core.NFData GetAccountSettingsResponse
