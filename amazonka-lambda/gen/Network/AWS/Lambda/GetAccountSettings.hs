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

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetAccountSettings' smart constructor.
data GetAccountSettings = GetAccountSettings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetAccountSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetAccountSettings ::
  GetAccountSettings
newGetAccountSettings = GetAccountSettings'

instance Prelude.AWSRequest GetAccountSettings where
  type
    Rs GetAccountSettings =
      GetAccountSettingsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAccountSettingsResponse'
            Prelude.<$> (x Prelude..?> "AccountLimit")
            Prelude.<*> (x Prelude..?> "AccountUsage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAccountSettings

instance Prelude.NFData GetAccountSettings

instance Prelude.ToHeaders GetAccountSettings where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetAccountSettings where
  toPath =
    Prelude.const "/2016-08-19/account-settings/"

instance Prelude.ToQuery GetAccountSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAccountSettingsResponse' smart constructor.
data GetAccountSettingsResponse = GetAccountSettingsResponse'
  { -- | Limits that are related to concurrency and code storage.
    accountLimit :: Prelude.Maybe AccountLimit,
    -- | The number of functions and amount of storage in use.
    accountUsage :: Prelude.Maybe AccountUsage,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetAccountSettingsResponse
newGetAccountSettingsResponse pHttpStatus_ =
  GetAccountSettingsResponse'
    { accountLimit =
        Prelude.Nothing,
      accountUsage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Limits that are related to concurrency and code storage.
getAccountSettingsResponse_accountLimit :: Lens.Lens' GetAccountSettingsResponse (Prelude.Maybe AccountLimit)
getAccountSettingsResponse_accountLimit = Lens.lens (\GetAccountSettingsResponse' {accountLimit} -> accountLimit) (\s@GetAccountSettingsResponse' {} a -> s {accountLimit = a} :: GetAccountSettingsResponse)

-- | The number of functions and amount of storage in use.
getAccountSettingsResponse_accountUsage :: Lens.Lens' GetAccountSettingsResponse (Prelude.Maybe AccountUsage)
getAccountSettingsResponse_accountUsage = Lens.lens (\GetAccountSettingsResponse' {accountUsage} -> accountUsage) (\s@GetAccountSettingsResponse' {} a -> s {accountUsage = a} :: GetAccountSettingsResponse)

-- | The response's http status code.
getAccountSettingsResponse_httpStatus :: Lens.Lens' GetAccountSettingsResponse Prelude.Int
getAccountSettingsResponse_httpStatus = Lens.lens (\GetAccountSettingsResponse' {httpStatus} -> httpStatus) (\s@GetAccountSettingsResponse' {} a -> s {httpStatus = a} :: GetAccountSettingsResponse)

instance Prelude.NFData GetAccountSettingsResponse
