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
-- Module      : Amazonka.SNS.DeleteSMSSandboxPhoneNumber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Web Services account\'s verified or pending phone
-- number from the SMS sandbox.
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
module Amazonka.SNS.DeleteSMSSandboxPhoneNumber
  ( -- * Creating a Request
    DeleteSMSSandboxPhoneNumber (..),
    newDeleteSMSSandboxPhoneNumber,

    -- * Request Lenses
    deleteSMSSandboxPhoneNumber_phoneNumber,

    -- * Destructuring the Response
    DeleteSMSSandboxPhoneNumberResponse (..),
    newDeleteSMSSandboxPhoneNumberResponse,

    -- * Response Lenses
    deleteSMSSandboxPhoneNumberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | /See:/ 'newDeleteSMSSandboxPhoneNumber' smart constructor.
data DeleteSMSSandboxPhoneNumber = DeleteSMSSandboxPhoneNumber'
  { -- | The destination phone number to delete.
    phoneNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSMSSandboxPhoneNumber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumber', 'deleteSMSSandboxPhoneNumber_phoneNumber' - The destination phone number to delete.
newDeleteSMSSandboxPhoneNumber ::
  -- | 'phoneNumber'
  Prelude.Text ->
  DeleteSMSSandboxPhoneNumber
newDeleteSMSSandboxPhoneNumber pPhoneNumber_ =
  DeleteSMSSandboxPhoneNumber'
    { phoneNumber =
        pPhoneNumber_
    }

-- | The destination phone number to delete.
deleteSMSSandboxPhoneNumber_phoneNumber :: Lens.Lens' DeleteSMSSandboxPhoneNumber Prelude.Text
deleteSMSSandboxPhoneNumber_phoneNumber = Lens.lens (\DeleteSMSSandboxPhoneNumber' {phoneNumber} -> phoneNumber) (\s@DeleteSMSSandboxPhoneNumber' {} a -> s {phoneNumber = a} :: DeleteSMSSandboxPhoneNumber)

instance Core.AWSRequest DeleteSMSSandboxPhoneNumber where
  type
    AWSResponse DeleteSMSSandboxPhoneNumber =
      DeleteSMSSandboxPhoneNumberResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteSMSSandboxPhoneNumberResult"
      ( \s h x ->
          DeleteSMSSandboxPhoneNumberResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSMSSandboxPhoneNumber where
  hashWithSalt _salt DeleteSMSSandboxPhoneNumber' {..} =
    _salt `Prelude.hashWithSalt` phoneNumber

instance Prelude.NFData DeleteSMSSandboxPhoneNumber where
  rnf DeleteSMSSandboxPhoneNumber' {..} =
    Prelude.rnf phoneNumber

instance Data.ToHeaders DeleteSMSSandboxPhoneNumber where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteSMSSandboxPhoneNumber where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteSMSSandboxPhoneNumber where
  toQuery DeleteSMSSandboxPhoneNumber' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeleteSMSSandboxPhoneNumber" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-03-31" :: Prelude.ByteString),
        "PhoneNumber" Data.=: phoneNumber
      ]

-- | /See:/ 'newDeleteSMSSandboxPhoneNumberResponse' smart constructor.
data DeleteSMSSandboxPhoneNumberResponse = DeleteSMSSandboxPhoneNumberResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSMSSandboxPhoneNumberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSMSSandboxPhoneNumberResponse_httpStatus' - The response's http status code.
newDeleteSMSSandboxPhoneNumberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSMSSandboxPhoneNumberResponse
newDeleteSMSSandboxPhoneNumberResponse pHttpStatus_ =
  DeleteSMSSandboxPhoneNumberResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSMSSandboxPhoneNumberResponse_httpStatus :: Lens.Lens' DeleteSMSSandboxPhoneNumberResponse Prelude.Int
deleteSMSSandboxPhoneNumberResponse_httpStatus = Lens.lens (\DeleteSMSSandboxPhoneNumberResponse' {httpStatus} -> httpStatus) (\s@DeleteSMSSandboxPhoneNumberResponse' {} a -> s {httpStatus = a} :: DeleteSMSSandboxPhoneNumberResponse)

instance
  Prelude.NFData
    DeleteSMSSandboxPhoneNumberResponse
  where
  rnf DeleteSMSSandboxPhoneNumberResponse' {..} =
    Prelude.rnf httpStatus
