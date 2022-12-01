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
-- Module      : Amazonka.Lambda.GetAccountSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details about your account\'s
-- <https://docs.aws.amazon.com/lambda/latest/dg/limits.html limits> and
-- usage in an Amazon Web Services Region.
module Amazonka.Lambda.GetAccountSettings
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAccountSettings' smart constructor.
data GetAccountSettings = GetAccountSettings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAccountSettingsResponse'
            Prelude.<$> (x Core..?> "AccountLimit")
            Prelude.<*> (x Core..?> "AccountUsage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAccountSettings where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetAccountSettings where
  rnf _ = ()

instance Core.ToHeaders GetAccountSettings where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetAccountSettings where
  toPath =
    Prelude.const "/2016-08-19/account-settings/"

instance Core.ToQuery GetAccountSettings where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData GetAccountSettingsResponse where
  rnf GetAccountSettingsResponse' {..} =
    Prelude.rnf accountLimit
      `Prelude.seq` Prelude.rnf accountUsage
      `Prelude.seq` Prelude.rnf httpStatus
