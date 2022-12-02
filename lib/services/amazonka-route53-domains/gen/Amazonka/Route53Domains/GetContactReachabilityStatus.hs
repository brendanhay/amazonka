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
-- Module      : Amazonka.Route53Domains.GetContactReachabilityStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For operations that require confirmation that the email address for the
-- registrant contact is valid, such as registering a new domain, this
-- operation returns information about whether the registrant contact has
-- responded.
--
-- If you want us to resend the email, use the
-- @ResendContactReachabilityEmail@ operation.
module Amazonka.Route53Domains.GetContactReachabilityStatus
  ( -- * Creating a Request
    GetContactReachabilityStatus (..),
    newGetContactReachabilityStatus,

    -- * Request Lenses
    getContactReachabilityStatus_domainName,

    -- * Destructuring the Response
    GetContactReachabilityStatusResponse (..),
    newGetContactReachabilityStatusResponse,

    -- * Response Lenses
    getContactReachabilityStatusResponse_domainName,
    getContactReachabilityStatusResponse_status,
    getContactReachabilityStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

-- | /See:/ 'newGetContactReachabilityStatus' smart constructor.
data GetContactReachabilityStatus = GetContactReachabilityStatus'
  { -- | The name of the domain for which you want to know whether the registrant
    -- contact has confirmed that the email address is valid.
    domainName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContactReachabilityStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'getContactReachabilityStatus_domainName' - The name of the domain for which you want to know whether the registrant
-- contact has confirmed that the email address is valid.
newGetContactReachabilityStatus ::
  GetContactReachabilityStatus
newGetContactReachabilityStatus =
  GetContactReachabilityStatus'
    { domainName =
        Prelude.Nothing
    }

-- | The name of the domain for which you want to know whether the registrant
-- contact has confirmed that the email address is valid.
getContactReachabilityStatus_domainName :: Lens.Lens' GetContactReachabilityStatus (Prelude.Maybe Prelude.Text)
getContactReachabilityStatus_domainName = Lens.lens (\GetContactReachabilityStatus' {domainName} -> domainName) (\s@GetContactReachabilityStatus' {} a -> s {domainName = a} :: GetContactReachabilityStatus)

instance Core.AWSRequest GetContactReachabilityStatus where
  type
    AWSResponse GetContactReachabilityStatus =
      GetContactReachabilityStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContactReachabilityStatusResponse'
            Prelude.<$> (x Data..?> "domainName")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetContactReachabilityStatus
  where
  hashWithSalt _salt GetContactReachabilityStatus' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData GetContactReachabilityStatus where
  rnf GetContactReachabilityStatus' {..} =
    Prelude.rnf domainName

instance Data.ToHeaders GetContactReachabilityStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.GetContactReachabilityStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetContactReachabilityStatus where
  toJSON GetContactReachabilityStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [("domainName" Data..=) Prelude.<$> domainName]
      )

instance Data.ToPath GetContactReachabilityStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery GetContactReachabilityStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetContactReachabilityStatusResponse' smart constructor.
data GetContactReachabilityStatusResponse = GetContactReachabilityStatusResponse'
  { -- | The domain name for which you requested the reachability status.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | Whether the registrant contact has responded. Values include the
    -- following:
    --
    -- [PENDING]
    --     We sent the confirmation email and haven\'t received a response yet.
    --
    -- [DONE]
    --     We sent the email and got confirmation from the registrant contact.
    --
    -- [EXPIRED]
    --     The time limit expired before the registrant contact responded.
    status :: Prelude.Maybe ReachabilityStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContactReachabilityStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'getContactReachabilityStatusResponse_domainName' - The domain name for which you requested the reachability status.
--
-- 'status', 'getContactReachabilityStatusResponse_status' - Whether the registrant contact has responded. Values include the
-- following:
--
-- [PENDING]
--     We sent the confirmation email and haven\'t received a response yet.
--
-- [DONE]
--     We sent the email and got confirmation from the registrant contact.
--
-- [EXPIRED]
--     The time limit expired before the registrant contact responded.
--
-- 'httpStatus', 'getContactReachabilityStatusResponse_httpStatus' - The response's http status code.
newGetContactReachabilityStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetContactReachabilityStatusResponse
newGetContactReachabilityStatusResponse pHttpStatus_ =
  GetContactReachabilityStatusResponse'
    { domainName =
        Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The domain name for which you requested the reachability status.
getContactReachabilityStatusResponse_domainName :: Lens.Lens' GetContactReachabilityStatusResponse (Prelude.Maybe Prelude.Text)
getContactReachabilityStatusResponse_domainName = Lens.lens (\GetContactReachabilityStatusResponse' {domainName} -> domainName) (\s@GetContactReachabilityStatusResponse' {} a -> s {domainName = a} :: GetContactReachabilityStatusResponse)

-- | Whether the registrant contact has responded. Values include the
-- following:
--
-- [PENDING]
--     We sent the confirmation email and haven\'t received a response yet.
--
-- [DONE]
--     We sent the email and got confirmation from the registrant contact.
--
-- [EXPIRED]
--     The time limit expired before the registrant contact responded.
getContactReachabilityStatusResponse_status :: Lens.Lens' GetContactReachabilityStatusResponse (Prelude.Maybe ReachabilityStatus)
getContactReachabilityStatusResponse_status = Lens.lens (\GetContactReachabilityStatusResponse' {status} -> status) (\s@GetContactReachabilityStatusResponse' {} a -> s {status = a} :: GetContactReachabilityStatusResponse)

-- | The response's http status code.
getContactReachabilityStatusResponse_httpStatus :: Lens.Lens' GetContactReachabilityStatusResponse Prelude.Int
getContactReachabilityStatusResponse_httpStatus = Lens.lens (\GetContactReachabilityStatusResponse' {httpStatus} -> httpStatus) (\s@GetContactReachabilityStatusResponse' {} a -> s {httpStatus = a} :: GetContactReachabilityStatusResponse)

instance
  Prelude.NFData
    GetContactReachabilityStatusResponse
  where
  rnf GetContactReachabilityStatusResponse' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
