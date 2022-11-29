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
-- Module      : Amazonka.SES.GetAccountSendingEnabled
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the email sending status of the Amazon SES account for the
-- current region.
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.GetAccountSendingEnabled
  ( -- * Creating a Request
    GetAccountSendingEnabled (..),
    newGetAccountSendingEnabled,

    -- * Destructuring the Response
    GetAccountSendingEnabledResponse (..),
    newGetAccountSendingEnabledResponse,

    -- * Response Lenses
    getAccountSendingEnabledResponse_enabled,
    getAccountSendingEnabledResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | /See:/ 'newGetAccountSendingEnabled' smart constructor.
data GetAccountSendingEnabled = GetAccountSendingEnabled'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccountSendingEnabled' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetAccountSendingEnabled ::
  GetAccountSendingEnabled
newGetAccountSendingEnabled =
  GetAccountSendingEnabled'

instance Core.AWSRequest GetAccountSendingEnabled where
  type
    AWSResponse GetAccountSendingEnabled =
      GetAccountSendingEnabledResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetAccountSendingEnabledResult"
      ( \s h x ->
          GetAccountSendingEnabledResponse'
            Prelude.<$> (x Core..@? "Enabled")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAccountSendingEnabled where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetAccountSendingEnabled where
  rnf _ = ()

instance Core.ToHeaders GetAccountSendingEnabled where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetAccountSendingEnabled where
  toPath = Prelude.const "/"

instance Core.ToQuery GetAccountSendingEnabled where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Core.=: ("GetAccountSendingEnabled" :: Prelude.ByteString),
            "Version"
              Core.=: ("2010-12-01" :: Prelude.ByteString)
          ]
      )

-- | Represents a request to return the email sending status for your Amazon
-- SES account in the current AWS Region.
--
-- /See:/ 'newGetAccountSendingEnabledResponse' smart constructor.
data GetAccountSendingEnabledResponse = GetAccountSendingEnabledResponse'
  { -- | Describes whether email sending is enabled or disabled for your Amazon
    -- SES account in the current AWS Region.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccountSendingEnabledResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'getAccountSendingEnabledResponse_enabled' - Describes whether email sending is enabled or disabled for your Amazon
-- SES account in the current AWS Region.
--
-- 'httpStatus', 'getAccountSendingEnabledResponse_httpStatus' - The response's http status code.
newGetAccountSendingEnabledResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAccountSendingEnabledResponse
newGetAccountSendingEnabledResponse pHttpStatus_ =
  GetAccountSendingEnabledResponse'
    { enabled =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Describes whether email sending is enabled or disabled for your Amazon
-- SES account in the current AWS Region.
getAccountSendingEnabledResponse_enabled :: Lens.Lens' GetAccountSendingEnabledResponse (Prelude.Maybe Prelude.Bool)
getAccountSendingEnabledResponse_enabled = Lens.lens (\GetAccountSendingEnabledResponse' {enabled} -> enabled) (\s@GetAccountSendingEnabledResponse' {} a -> s {enabled = a} :: GetAccountSendingEnabledResponse)

-- | The response's http status code.
getAccountSendingEnabledResponse_httpStatus :: Lens.Lens' GetAccountSendingEnabledResponse Prelude.Int
getAccountSendingEnabledResponse_httpStatus = Lens.lens (\GetAccountSendingEnabledResponse' {httpStatus} -> httpStatus) (\s@GetAccountSendingEnabledResponse' {} a -> s {httpStatus = a} :: GetAccountSendingEnabledResponse)

instance
  Prelude.NFData
    GetAccountSendingEnabledResponse
  where
  rnf GetAccountSendingEnabledResponse' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf httpStatus
