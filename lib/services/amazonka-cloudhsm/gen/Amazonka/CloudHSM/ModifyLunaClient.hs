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
-- Module      : Amazonka.CloudHSM.ModifyLunaClient
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__. For more
-- information, see
-- <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs>,
-- the
-- <https://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide>,
-- and the
-- <https://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference>.
--
-- __For information about the current version of AWS CloudHSM__, see
-- <http://aws.amazon.com/cloudhsm/ AWS CloudHSM>, the
-- <https://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide>,
-- and the
-- <https://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference>.
--
-- Modifies the certificate used by the client.
--
-- This action can potentially start a workflow to install the new
-- certificate on the client\'s HSMs.
module Amazonka.CloudHSM.ModifyLunaClient
  ( -- * Creating a Request
    ModifyLunaClient (..),
    newModifyLunaClient,

    -- * Request Lenses
    modifyLunaClient_clientArn,
    modifyLunaClient_certificate,

    -- * Destructuring the Response
    ModifyLunaClientResponse (..),
    newModifyLunaClientResponse,

    -- * Response Lenses
    modifyLunaClientResponse_clientArn,
    modifyLunaClientResponse_httpStatus,
  )
where

import Amazonka.CloudHSM.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyLunaClient' smart constructor.
data ModifyLunaClient = ModifyLunaClient'
  { -- | The ARN of the client.
    clientArn :: Prelude.Text,
    -- | The new certificate for the client.
    certificate :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyLunaClient' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientArn', 'modifyLunaClient_clientArn' - The ARN of the client.
--
-- 'certificate', 'modifyLunaClient_certificate' - The new certificate for the client.
newModifyLunaClient ::
  -- | 'clientArn'
  Prelude.Text ->
  -- | 'certificate'
  Prelude.Text ->
  ModifyLunaClient
newModifyLunaClient pClientArn_ pCertificate_ =
  ModifyLunaClient'
    { clientArn = pClientArn_,
      certificate = pCertificate_
    }

-- | The ARN of the client.
modifyLunaClient_clientArn :: Lens.Lens' ModifyLunaClient Prelude.Text
modifyLunaClient_clientArn = Lens.lens (\ModifyLunaClient' {clientArn} -> clientArn) (\s@ModifyLunaClient' {} a -> s {clientArn = a} :: ModifyLunaClient)

-- | The new certificate for the client.
modifyLunaClient_certificate :: Lens.Lens' ModifyLunaClient Prelude.Text
modifyLunaClient_certificate = Lens.lens (\ModifyLunaClient' {certificate} -> certificate) (\s@ModifyLunaClient' {} a -> s {certificate = a} :: ModifyLunaClient)

instance Core.AWSRequest ModifyLunaClient where
  type
    AWSResponse ModifyLunaClient =
      ModifyLunaClientResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ModifyLunaClientResponse'
            Prelude.<$> (x Data..?> "ClientArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyLunaClient where
  hashWithSalt _salt ModifyLunaClient' {..} =
    _salt `Prelude.hashWithSalt` clientArn
      `Prelude.hashWithSalt` certificate

instance Prelude.NFData ModifyLunaClient where
  rnf ModifyLunaClient' {..} =
    Prelude.rnf clientArn
      `Prelude.seq` Prelude.rnf certificate

instance Data.ToHeaders ModifyLunaClient where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CloudHsmFrontendService.ModifyLunaClient" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ModifyLunaClient where
  toJSON ModifyLunaClient' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ClientArn" Data..= clientArn),
            Prelude.Just ("Certificate" Data..= certificate)
          ]
      )

instance Data.ToPath ModifyLunaClient where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyLunaClient where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newModifyLunaClientResponse' smart constructor.
data ModifyLunaClientResponse = ModifyLunaClientResponse'
  { -- | The ARN of the client.
    clientArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyLunaClientResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientArn', 'modifyLunaClientResponse_clientArn' - The ARN of the client.
--
-- 'httpStatus', 'modifyLunaClientResponse_httpStatus' - The response's http status code.
newModifyLunaClientResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyLunaClientResponse
newModifyLunaClientResponse pHttpStatus_ =
  ModifyLunaClientResponse'
    { clientArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the client.
modifyLunaClientResponse_clientArn :: Lens.Lens' ModifyLunaClientResponse (Prelude.Maybe Prelude.Text)
modifyLunaClientResponse_clientArn = Lens.lens (\ModifyLunaClientResponse' {clientArn} -> clientArn) (\s@ModifyLunaClientResponse' {} a -> s {clientArn = a} :: ModifyLunaClientResponse)

-- | The response's http status code.
modifyLunaClientResponse_httpStatus :: Lens.Lens' ModifyLunaClientResponse Prelude.Int
modifyLunaClientResponse_httpStatus = Lens.lens (\ModifyLunaClientResponse' {httpStatus} -> httpStatus) (\s@ModifyLunaClientResponse' {} a -> s {httpStatus = a} :: ModifyLunaClientResponse)

instance Prelude.NFData ModifyLunaClientResponse where
  rnf ModifyLunaClientResponse' {..} =
    Prelude.rnf clientArn
      `Prelude.seq` Prelude.rnf httpStatus
