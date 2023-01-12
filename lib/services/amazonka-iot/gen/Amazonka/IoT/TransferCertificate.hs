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
-- Module      : Amazonka.IoT.TransferCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Transfers the specified certificate to the specified Amazon Web Services
-- account.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions TransferCertificate>
-- action.
--
-- You can cancel the transfer until it is acknowledged by the recipient.
--
-- No notification is sent to the transfer destination\'s account. It is up
-- to the caller to notify the transfer target.
--
-- The certificate being transferred must not be in the ACTIVE state. You
-- can use the UpdateCertificate action to deactivate it.
--
-- The certificate must not have any policies attached to it. You can use
-- the DetachPolicy action to detach them.
module Amazonka.IoT.TransferCertificate
  ( -- * Creating a Request
    TransferCertificate (..),
    newTransferCertificate,

    -- * Request Lenses
    transferCertificate_transferMessage,
    transferCertificate_certificateId,
    transferCertificate_targetAwsAccount,

    -- * Destructuring the Response
    TransferCertificateResponse (..),
    newTransferCertificateResponse,

    -- * Response Lenses
    transferCertificateResponse_transferredCertificateArn,
    transferCertificateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the TransferCertificate operation.
--
-- /See:/ 'newTransferCertificate' smart constructor.
data TransferCertificate = TransferCertificate'
  { -- | The transfer message.
    transferMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the certificate. (The last part of the certificate ARN
    -- contains the certificate ID.)
    certificateId :: Prelude.Text,
    -- | The Amazon Web Services account.
    targetAwsAccount :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransferCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transferMessage', 'transferCertificate_transferMessage' - The transfer message.
--
-- 'certificateId', 'transferCertificate_certificateId' - The ID of the certificate. (The last part of the certificate ARN
-- contains the certificate ID.)
--
-- 'targetAwsAccount', 'transferCertificate_targetAwsAccount' - The Amazon Web Services account.
newTransferCertificate ::
  -- | 'certificateId'
  Prelude.Text ->
  -- | 'targetAwsAccount'
  Prelude.Text ->
  TransferCertificate
newTransferCertificate
  pCertificateId_
  pTargetAwsAccount_ =
    TransferCertificate'
      { transferMessage =
          Prelude.Nothing,
        certificateId = pCertificateId_,
        targetAwsAccount = pTargetAwsAccount_
      }

-- | The transfer message.
transferCertificate_transferMessage :: Lens.Lens' TransferCertificate (Prelude.Maybe Prelude.Text)
transferCertificate_transferMessage = Lens.lens (\TransferCertificate' {transferMessage} -> transferMessage) (\s@TransferCertificate' {} a -> s {transferMessage = a} :: TransferCertificate)

-- | The ID of the certificate. (The last part of the certificate ARN
-- contains the certificate ID.)
transferCertificate_certificateId :: Lens.Lens' TransferCertificate Prelude.Text
transferCertificate_certificateId = Lens.lens (\TransferCertificate' {certificateId} -> certificateId) (\s@TransferCertificate' {} a -> s {certificateId = a} :: TransferCertificate)

-- | The Amazon Web Services account.
transferCertificate_targetAwsAccount :: Lens.Lens' TransferCertificate Prelude.Text
transferCertificate_targetAwsAccount = Lens.lens (\TransferCertificate' {targetAwsAccount} -> targetAwsAccount) (\s@TransferCertificate' {} a -> s {targetAwsAccount = a} :: TransferCertificate)

instance Core.AWSRequest TransferCertificate where
  type
    AWSResponse TransferCertificate =
      TransferCertificateResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          TransferCertificateResponse'
            Prelude.<$> (x Data..?> "transferredCertificateArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TransferCertificate where
  hashWithSalt _salt TransferCertificate' {..} =
    _salt `Prelude.hashWithSalt` transferMessage
      `Prelude.hashWithSalt` certificateId
      `Prelude.hashWithSalt` targetAwsAccount

instance Prelude.NFData TransferCertificate where
  rnf TransferCertificate' {..} =
    Prelude.rnf transferMessage
      `Prelude.seq` Prelude.rnf certificateId
      `Prelude.seq` Prelude.rnf targetAwsAccount

instance Data.ToHeaders TransferCertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON TransferCertificate where
  toJSON TransferCertificate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("transferMessage" Data..=)
              Prelude.<$> transferMessage
          ]
      )

instance Data.ToPath TransferCertificate where
  toPath TransferCertificate' {..} =
    Prelude.mconcat
      ["/transfer-certificate/", Data.toBS certificateId]

instance Data.ToQuery TransferCertificate where
  toQuery TransferCertificate' {..} =
    Prelude.mconcat
      ["targetAwsAccount" Data.=: targetAwsAccount]

-- | The output from the TransferCertificate operation.
--
-- /See:/ 'newTransferCertificateResponse' smart constructor.
data TransferCertificateResponse = TransferCertificateResponse'
  { -- | The ARN of the certificate.
    transferredCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransferCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transferredCertificateArn', 'transferCertificateResponse_transferredCertificateArn' - The ARN of the certificate.
--
-- 'httpStatus', 'transferCertificateResponse_httpStatus' - The response's http status code.
newTransferCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TransferCertificateResponse
newTransferCertificateResponse pHttpStatus_ =
  TransferCertificateResponse'
    { transferredCertificateArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the certificate.
transferCertificateResponse_transferredCertificateArn :: Lens.Lens' TransferCertificateResponse (Prelude.Maybe Prelude.Text)
transferCertificateResponse_transferredCertificateArn = Lens.lens (\TransferCertificateResponse' {transferredCertificateArn} -> transferredCertificateArn) (\s@TransferCertificateResponse' {} a -> s {transferredCertificateArn = a} :: TransferCertificateResponse)

-- | The response's http status code.
transferCertificateResponse_httpStatus :: Lens.Lens' TransferCertificateResponse Prelude.Int
transferCertificateResponse_httpStatus = Lens.lens (\TransferCertificateResponse' {httpStatus} -> httpStatus) (\s@TransferCertificateResponse' {} a -> s {httpStatus = a} :: TransferCertificateResponse)

instance Prelude.NFData TransferCertificateResponse where
  rnf TransferCertificateResponse' {..} =
    Prelude.rnf transferredCertificateArn
      `Prelude.seq` Prelude.rnf httpStatus
