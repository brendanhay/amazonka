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
-- Module      : Amazonka.SNS.GetSMSSandboxAccountStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the SMS sandbox status for the calling Amazon Web Services
-- account in the target Amazon Web Services Region.
--
-- When you start using Amazon SNS to send SMS messages, your Amazon Web
-- Services account is in the /SMS sandbox/. The SMS sandbox provides a
-- safe environment for you to try Amazon SNS features without risking your
-- reputation as an SMS sender. While your Amazon Web Services account is
-- in the SMS sandbox, you can use all of the features of Amazon SNS.
-- However, you can send SMS messages only to verified destination phone
-- numbers. For more information, including how to move out of the sandbox
-- to send messages without restrictions, see
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-sms-sandbox.html SMS sandbox>
-- in the /Amazon SNS Developer Guide/.
module Amazonka.SNS.GetSMSSandboxAccountStatus
  ( -- * Creating a Request
    GetSMSSandboxAccountStatus (..),
    newGetSMSSandboxAccountStatus,

    -- * Destructuring the Response
    GetSMSSandboxAccountStatusResponse (..),
    newGetSMSSandboxAccountStatusResponse,

    -- * Response Lenses
    getSMSSandboxAccountStatusResponse_httpStatus,
    getSMSSandboxAccountStatusResponse_isInSandbox,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | /See:/ 'newGetSMSSandboxAccountStatus' smart constructor.
data GetSMSSandboxAccountStatus = GetSMSSandboxAccountStatus'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSMSSandboxAccountStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetSMSSandboxAccountStatus ::
  GetSMSSandboxAccountStatus
newGetSMSSandboxAccountStatus =
  GetSMSSandboxAccountStatus'

instance Core.AWSRequest GetSMSSandboxAccountStatus where
  type
    AWSResponse GetSMSSandboxAccountStatus =
      GetSMSSandboxAccountStatusResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetSMSSandboxAccountStatusResult"
      ( \s h x ->
          GetSMSSandboxAccountStatusResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "IsInSandbox")
      )

instance Prelude.Hashable GetSMSSandboxAccountStatus where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetSMSSandboxAccountStatus where
  rnf _ = ()

instance Data.ToHeaders GetSMSSandboxAccountStatus where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetSMSSandboxAccountStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSMSSandboxAccountStatus where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Data.=: ("GetSMSSandboxAccountStatus" :: Prelude.ByteString),
            "Version"
              Data.=: ("2010-03-31" :: Prelude.ByteString)
          ]
      )

-- | /See:/ 'newGetSMSSandboxAccountStatusResponse' smart constructor.
data GetSMSSandboxAccountStatusResponse = GetSMSSandboxAccountStatusResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Indicates whether the calling Amazon Web Services account is in the SMS
    -- sandbox.
    isInSandbox :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSMSSandboxAccountStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getSMSSandboxAccountStatusResponse_httpStatus' - The response's http status code.
--
-- 'isInSandbox', 'getSMSSandboxAccountStatusResponse_isInSandbox' - Indicates whether the calling Amazon Web Services account is in the SMS
-- sandbox.
newGetSMSSandboxAccountStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'isInSandbox'
  Prelude.Bool ->
  GetSMSSandboxAccountStatusResponse
newGetSMSSandboxAccountStatusResponse
  pHttpStatus_
  pIsInSandbox_ =
    GetSMSSandboxAccountStatusResponse'
      { httpStatus =
          pHttpStatus_,
        isInSandbox = pIsInSandbox_
      }

-- | The response's http status code.
getSMSSandboxAccountStatusResponse_httpStatus :: Lens.Lens' GetSMSSandboxAccountStatusResponse Prelude.Int
getSMSSandboxAccountStatusResponse_httpStatus = Lens.lens (\GetSMSSandboxAccountStatusResponse' {httpStatus} -> httpStatus) (\s@GetSMSSandboxAccountStatusResponse' {} a -> s {httpStatus = a} :: GetSMSSandboxAccountStatusResponse)

-- | Indicates whether the calling Amazon Web Services account is in the SMS
-- sandbox.
getSMSSandboxAccountStatusResponse_isInSandbox :: Lens.Lens' GetSMSSandboxAccountStatusResponse Prelude.Bool
getSMSSandboxAccountStatusResponse_isInSandbox = Lens.lens (\GetSMSSandboxAccountStatusResponse' {isInSandbox} -> isInSandbox) (\s@GetSMSSandboxAccountStatusResponse' {} a -> s {isInSandbox = a} :: GetSMSSandboxAccountStatusResponse)

instance
  Prelude.NFData
    GetSMSSandboxAccountStatusResponse
  where
  rnf GetSMSSandboxAccountStatusResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf isInSandbox
