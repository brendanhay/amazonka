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
-- Module      : Network.AWS.SES.GetAccountSendingEnabled
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the email sending status of the Amazon SES account for the
-- current region.
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.GetAccountSendingEnabled
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SES.Types

-- | /See:/ 'newGetAccountSendingEnabled' smart constructor.
data GetAccountSendingEnabled = GetAccountSendingEnabled'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetAccountSendingEnabled' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetAccountSendingEnabled ::
  GetAccountSendingEnabled
newGetAccountSendingEnabled =
  GetAccountSendingEnabled'

instance Prelude.AWSRequest GetAccountSendingEnabled where
  type
    Rs GetAccountSendingEnabled =
      GetAccountSendingEnabledResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetAccountSendingEnabledResult"
      ( \s h x ->
          GetAccountSendingEnabledResponse'
            Prelude.<$> (x Prelude..@? "Enabled")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAccountSendingEnabled

instance Prelude.NFData GetAccountSendingEnabled

instance Prelude.ToHeaders GetAccountSendingEnabled where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetAccountSendingEnabled where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetAccountSendingEnabled where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Prelude.=: ("GetAccountSendingEnabled" :: Prelude.ByteString),
            "Version"
              Prelude.=: ("2010-12-01" :: Prelude.ByteString)
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
