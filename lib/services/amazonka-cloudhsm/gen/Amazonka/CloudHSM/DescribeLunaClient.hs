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
-- Module      : Amazonka.CloudHSM.DescribeLunaClient
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- Retrieves information about an HSM client.
module Amazonka.CloudHSM.DescribeLunaClient
  ( -- * Creating a Request
    DescribeLunaClient (..),
    newDescribeLunaClient,

    -- * Request Lenses
    describeLunaClient_clientArn,
    describeLunaClient_certificateFingerprint,

    -- * Destructuring the Response
    DescribeLunaClientResponse (..),
    newDescribeLunaClientResponse,

    -- * Response Lenses
    describeLunaClientResponse_label,
    describeLunaClientResponse_lastModifiedTimestamp,
    describeLunaClientResponse_clientArn,
    describeLunaClientResponse_certificate,
    describeLunaClientResponse_certificateFingerprint,
    describeLunaClientResponse_httpStatus,
  )
where

import Amazonka.CloudHSM.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLunaClient' smart constructor.
data DescribeLunaClient = DescribeLunaClient'
  { -- | The ARN of the client.
    clientArn :: Prelude.Maybe Prelude.Text,
    -- | The certificate fingerprint.
    certificateFingerprint :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLunaClient' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientArn', 'describeLunaClient_clientArn' - The ARN of the client.
--
-- 'certificateFingerprint', 'describeLunaClient_certificateFingerprint' - The certificate fingerprint.
newDescribeLunaClient ::
  DescribeLunaClient
newDescribeLunaClient =
  DescribeLunaClient'
    { clientArn = Prelude.Nothing,
      certificateFingerprint = Prelude.Nothing
    }

-- | The ARN of the client.
describeLunaClient_clientArn :: Lens.Lens' DescribeLunaClient (Prelude.Maybe Prelude.Text)
describeLunaClient_clientArn = Lens.lens (\DescribeLunaClient' {clientArn} -> clientArn) (\s@DescribeLunaClient' {} a -> s {clientArn = a} :: DescribeLunaClient)

-- | The certificate fingerprint.
describeLunaClient_certificateFingerprint :: Lens.Lens' DescribeLunaClient (Prelude.Maybe Prelude.Text)
describeLunaClient_certificateFingerprint = Lens.lens (\DescribeLunaClient' {certificateFingerprint} -> certificateFingerprint) (\s@DescribeLunaClient' {} a -> s {certificateFingerprint = a} :: DescribeLunaClient)

instance Core.AWSRequest DescribeLunaClient where
  type
    AWSResponse DescribeLunaClient =
      DescribeLunaClientResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLunaClientResponse'
            Prelude.<$> (x Data..?> "Label")
            Prelude.<*> (x Data..?> "LastModifiedTimestamp")
            Prelude.<*> (x Data..?> "ClientArn")
            Prelude.<*> (x Data..?> "Certificate")
            Prelude.<*> (x Data..?> "CertificateFingerprint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLunaClient where
  hashWithSalt _salt DescribeLunaClient' {..} =
    _salt `Prelude.hashWithSalt` clientArn
      `Prelude.hashWithSalt` certificateFingerprint

instance Prelude.NFData DescribeLunaClient where
  rnf DescribeLunaClient' {..} =
    Prelude.rnf clientArn
      `Prelude.seq` Prelude.rnf certificateFingerprint

instance Data.ToHeaders DescribeLunaClient where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CloudHsmFrontendService.DescribeLunaClient" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeLunaClient where
  toJSON DescribeLunaClient' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientArn" Data..=) Prelude.<$> clientArn,
            ("CertificateFingerprint" Data..=)
              Prelude.<$> certificateFingerprint
          ]
      )

instance Data.ToPath DescribeLunaClient where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeLunaClient where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeLunaClientResponse' smart constructor.
data DescribeLunaClientResponse = DescribeLunaClientResponse'
  { -- | The label of the client.
    label :: Prelude.Maybe Prelude.Text,
    -- | The date and time the client was last modified.
    lastModifiedTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the client.
    clientArn :: Prelude.Maybe Prelude.Text,
    -- | The certificate installed on the HSMs used by this client.
    certificate :: Prelude.Maybe Prelude.Text,
    -- | The certificate fingerprint.
    certificateFingerprint :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLunaClientResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'label', 'describeLunaClientResponse_label' - The label of the client.
--
-- 'lastModifiedTimestamp', 'describeLunaClientResponse_lastModifiedTimestamp' - The date and time the client was last modified.
--
-- 'clientArn', 'describeLunaClientResponse_clientArn' - The ARN of the client.
--
-- 'certificate', 'describeLunaClientResponse_certificate' - The certificate installed on the HSMs used by this client.
--
-- 'certificateFingerprint', 'describeLunaClientResponse_certificateFingerprint' - The certificate fingerprint.
--
-- 'httpStatus', 'describeLunaClientResponse_httpStatus' - The response's http status code.
newDescribeLunaClientResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLunaClientResponse
newDescribeLunaClientResponse pHttpStatus_ =
  DescribeLunaClientResponse'
    { label =
        Prelude.Nothing,
      lastModifiedTimestamp = Prelude.Nothing,
      clientArn = Prelude.Nothing,
      certificate = Prelude.Nothing,
      certificateFingerprint = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The label of the client.
describeLunaClientResponse_label :: Lens.Lens' DescribeLunaClientResponse (Prelude.Maybe Prelude.Text)
describeLunaClientResponse_label = Lens.lens (\DescribeLunaClientResponse' {label} -> label) (\s@DescribeLunaClientResponse' {} a -> s {label = a} :: DescribeLunaClientResponse)

-- | The date and time the client was last modified.
describeLunaClientResponse_lastModifiedTimestamp :: Lens.Lens' DescribeLunaClientResponse (Prelude.Maybe Prelude.Text)
describeLunaClientResponse_lastModifiedTimestamp = Lens.lens (\DescribeLunaClientResponse' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@DescribeLunaClientResponse' {} a -> s {lastModifiedTimestamp = a} :: DescribeLunaClientResponse)

-- | The ARN of the client.
describeLunaClientResponse_clientArn :: Lens.Lens' DescribeLunaClientResponse (Prelude.Maybe Prelude.Text)
describeLunaClientResponse_clientArn = Lens.lens (\DescribeLunaClientResponse' {clientArn} -> clientArn) (\s@DescribeLunaClientResponse' {} a -> s {clientArn = a} :: DescribeLunaClientResponse)

-- | The certificate installed on the HSMs used by this client.
describeLunaClientResponse_certificate :: Lens.Lens' DescribeLunaClientResponse (Prelude.Maybe Prelude.Text)
describeLunaClientResponse_certificate = Lens.lens (\DescribeLunaClientResponse' {certificate} -> certificate) (\s@DescribeLunaClientResponse' {} a -> s {certificate = a} :: DescribeLunaClientResponse)

-- | The certificate fingerprint.
describeLunaClientResponse_certificateFingerprint :: Lens.Lens' DescribeLunaClientResponse (Prelude.Maybe Prelude.Text)
describeLunaClientResponse_certificateFingerprint = Lens.lens (\DescribeLunaClientResponse' {certificateFingerprint} -> certificateFingerprint) (\s@DescribeLunaClientResponse' {} a -> s {certificateFingerprint = a} :: DescribeLunaClientResponse)

-- | The response's http status code.
describeLunaClientResponse_httpStatus :: Lens.Lens' DescribeLunaClientResponse Prelude.Int
describeLunaClientResponse_httpStatus = Lens.lens (\DescribeLunaClientResponse' {httpStatus} -> httpStatus) (\s@DescribeLunaClientResponse' {} a -> s {httpStatus = a} :: DescribeLunaClientResponse)

instance Prelude.NFData DescribeLunaClientResponse where
  rnf DescribeLunaClientResponse' {..} =
    Prelude.rnf label
      `Prelude.seq` Prelude.rnf lastModifiedTimestamp
      `Prelude.seq` Prelude.rnf clientArn
      `Prelude.seq` Prelude.rnf certificate
      `Prelude.seq` Prelude.rnf certificateFingerprint
      `Prelude.seq` Prelude.rnf httpStatus
