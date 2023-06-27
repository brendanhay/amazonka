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
-- Module      : Amazonka.CloudHSM.CreateLunaClient
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
-- Creates an HSM client.
module Amazonka.CloudHSM.CreateLunaClient
  ( -- * Creating a Request
    CreateLunaClient (..),
    newCreateLunaClient,

    -- * Request Lenses
    createLunaClient_label,
    createLunaClient_certificate,

    -- * Destructuring the Response
    CreateLunaClientResponse (..),
    newCreateLunaClientResponse,

    -- * Response Lenses
    createLunaClientResponse_clientArn,
    createLunaClientResponse_httpStatus,
  )
where

import Amazonka.CloudHSM.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the inputs for the CreateLunaClient action.
--
-- /See:/ 'newCreateLunaClient' smart constructor.
data CreateLunaClient = CreateLunaClient'
  { -- | The label for the client.
    label :: Prelude.Maybe Prelude.Text,
    -- | The contents of a Base64-Encoded X.509 v3 certificate to be installed on
    -- the HSMs used by this client.
    certificate :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLunaClient' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'label', 'createLunaClient_label' - The label for the client.
--
-- 'certificate', 'createLunaClient_certificate' - The contents of a Base64-Encoded X.509 v3 certificate to be installed on
-- the HSMs used by this client.
newCreateLunaClient ::
  -- | 'certificate'
  Prelude.Text ->
  CreateLunaClient
newCreateLunaClient pCertificate_ =
  CreateLunaClient'
    { label = Prelude.Nothing,
      certificate = pCertificate_
    }

-- | The label for the client.
createLunaClient_label :: Lens.Lens' CreateLunaClient (Prelude.Maybe Prelude.Text)
createLunaClient_label = Lens.lens (\CreateLunaClient' {label} -> label) (\s@CreateLunaClient' {} a -> s {label = a} :: CreateLunaClient)

-- | The contents of a Base64-Encoded X.509 v3 certificate to be installed on
-- the HSMs used by this client.
createLunaClient_certificate :: Lens.Lens' CreateLunaClient Prelude.Text
createLunaClient_certificate = Lens.lens (\CreateLunaClient' {certificate} -> certificate) (\s@CreateLunaClient' {} a -> s {certificate = a} :: CreateLunaClient)

instance Core.AWSRequest CreateLunaClient where
  type
    AWSResponse CreateLunaClient =
      CreateLunaClientResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLunaClientResponse'
            Prelude.<$> (x Data..?> "ClientArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLunaClient where
  hashWithSalt _salt CreateLunaClient' {..} =
    _salt
      `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` certificate

instance Prelude.NFData CreateLunaClient where
  rnf CreateLunaClient' {..} =
    Prelude.rnf label
      `Prelude.seq` Prelude.rnf certificate

instance Data.ToHeaders CreateLunaClient where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CloudHsmFrontendService.CreateLunaClient" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLunaClient where
  toJSON CreateLunaClient' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Label" Data..=) Prelude.<$> label,
            Prelude.Just ("Certificate" Data..= certificate)
          ]
      )

instance Data.ToPath CreateLunaClient where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLunaClient where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of the CreateLunaClient action.
--
-- /See:/ 'newCreateLunaClientResponse' smart constructor.
data CreateLunaClientResponse = CreateLunaClientResponse'
  { -- | The ARN of the client.
    clientArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLunaClientResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientArn', 'createLunaClientResponse_clientArn' - The ARN of the client.
--
-- 'httpStatus', 'createLunaClientResponse_httpStatus' - The response's http status code.
newCreateLunaClientResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLunaClientResponse
newCreateLunaClientResponse pHttpStatus_ =
  CreateLunaClientResponse'
    { clientArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the client.
createLunaClientResponse_clientArn :: Lens.Lens' CreateLunaClientResponse (Prelude.Maybe Prelude.Text)
createLunaClientResponse_clientArn = Lens.lens (\CreateLunaClientResponse' {clientArn} -> clientArn) (\s@CreateLunaClientResponse' {} a -> s {clientArn = a} :: CreateLunaClientResponse)

-- | The response's http status code.
createLunaClientResponse_httpStatus :: Lens.Lens' CreateLunaClientResponse Prelude.Int
createLunaClientResponse_httpStatus = Lens.lens (\CreateLunaClientResponse' {httpStatus} -> httpStatus) (\s@CreateLunaClientResponse' {} a -> s {httpStatus = a} :: CreateLunaClientResponse)

instance Prelude.NFData CreateLunaClientResponse where
  rnf CreateLunaClientResponse' {..} =
    Prelude.rnf clientArn
      `Prelude.seq` Prelude.rnf httpStatus
