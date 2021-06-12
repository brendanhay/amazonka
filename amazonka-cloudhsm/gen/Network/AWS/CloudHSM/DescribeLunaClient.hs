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
-- Module      : Network.AWS.CloudHSM.DescribeLunaClient
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__. For more
-- information, see
-- <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs>,
-- the
-- <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide>,
-- and the
-- <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference>.
--
-- __For information about the current version of AWS CloudHSM__, see
-- <http://aws.amazon.com/cloudhsm/ AWS CloudHSM>, the
-- <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide>,
-- and the
-- <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference>.
--
-- Retrieves information about an HSM client.
module Network.AWS.CloudHSM.DescribeLunaClient
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
    describeLunaClientResponse_lastModifiedTimestamp,
    describeLunaClientResponse_clientArn,
    describeLunaClientResponse_label,
    describeLunaClientResponse_certificate,
    describeLunaClientResponse_certificateFingerprint,
    describeLunaClientResponse_httpStatus,
  )
where

import Network.AWS.CloudHSM.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLunaClient' smart constructor.
data DescribeLunaClient = DescribeLunaClient'
  { -- | The ARN of the client.
    clientArn :: Core.Maybe Core.Text,
    -- | The certificate fingerprint.
    certificateFingerprint :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { clientArn = Core.Nothing,
      certificateFingerprint = Core.Nothing
    }

-- | The ARN of the client.
describeLunaClient_clientArn :: Lens.Lens' DescribeLunaClient (Core.Maybe Core.Text)
describeLunaClient_clientArn = Lens.lens (\DescribeLunaClient' {clientArn} -> clientArn) (\s@DescribeLunaClient' {} a -> s {clientArn = a} :: DescribeLunaClient)

-- | The certificate fingerprint.
describeLunaClient_certificateFingerprint :: Lens.Lens' DescribeLunaClient (Core.Maybe Core.Text)
describeLunaClient_certificateFingerprint = Lens.lens (\DescribeLunaClient' {certificateFingerprint} -> certificateFingerprint) (\s@DescribeLunaClient' {} a -> s {certificateFingerprint = a} :: DescribeLunaClient)

instance Core.AWSRequest DescribeLunaClient where
  type
    AWSResponse DescribeLunaClient =
      DescribeLunaClientResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLunaClientResponse'
            Core.<$> (x Core..?> "LastModifiedTimestamp")
            Core.<*> (x Core..?> "ClientArn")
            Core.<*> (x Core..?> "Label")
            Core.<*> (x Core..?> "Certificate")
            Core.<*> (x Core..?> "CertificateFingerprint")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeLunaClient

instance Core.NFData DescribeLunaClient

instance Core.ToHeaders DescribeLunaClient where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CloudHsmFrontendService.DescribeLunaClient" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeLunaClient where
  toJSON DescribeLunaClient' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ClientArn" Core..=) Core.<$> clientArn,
            ("CertificateFingerprint" Core..=)
              Core.<$> certificateFingerprint
          ]
      )

instance Core.ToPath DescribeLunaClient where
  toPath = Core.const "/"

instance Core.ToQuery DescribeLunaClient where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeLunaClientResponse' smart constructor.
data DescribeLunaClientResponse = DescribeLunaClientResponse'
  { -- | The date and time the client was last modified.
    lastModifiedTimestamp :: Core.Maybe Core.Text,
    -- | The ARN of the client.
    clientArn :: Core.Maybe Core.Text,
    -- | The label of the client.
    label :: Core.Maybe Core.Text,
    -- | The certificate installed on the HSMs used by this client.
    certificate :: Core.Maybe Core.Text,
    -- | The certificate fingerprint.
    certificateFingerprint :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLunaClientResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTimestamp', 'describeLunaClientResponse_lastModifiedTimestamp' - The date and time the client was last modified.
--
-- 'clientArn', 'describeLunaClientResponse_clientArn' - The ARN of the client.
--
-- 'label', 'describeLunaClientResponse_label' - The label of the client.
--
-- 'certificate', 'describeLunaClientResponse_certificate' - The certificate installed on the HSMs used by this client.
--
-- 'certificateFingerprint', 'describeLunaClientResponse_certificateFingerprint' - The certificate fingerprint.
--
-- 'httpStatus', 'describeLunaClientResponse_httpStatus' - The response's http status code.
newDescribeLunaClientResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeLunaClientResponse
newDescribeLunaClientResponse pHttpStatus_ =
  DescribeLunaClientResponse'
    { lastModifiedTimestamp =
        Core.Nothing,
      clientArn = Core.Nothing,
      label = Core.Nothing,
      certificate = Core.Nothing,
      certificateFingerprint = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time the client was last modified.
describeLunaClientResponse_lastModifiedTimestamp :: Lens.Lens' DescribeLunaClientResponse (Core.Maybe Core.Text)
describeLunaClientResponse_lastModifiedTimestamp = Lens.lens (\DescribeLunaClientResponse' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@DescribeLunaClientResponse' {} a -> s {lastModifiedTimestamp = a} :: DescribeLunaClientResponse)

-- | The ARN of the client.
describeLunaClientResponse_clientArn :: Lens.Lens' DescribeLunaClientResponse (Core.Maybe Core.Text)
describeLunaClientResponse_clientArn = Lens.lens (\DescribeLunaClientResponse' {clientArn} -> clientArn) (\s@DescribeLunaClientResponse' {} a -> s {clientArn = a} :: DescribeLunaClientResponse)

-- | The label of the client.
describeLunaClientResponse_label :: Lens.Lens' DescribeLunaClientResponse (Core.Maybe Core.Text)
describeLunaClientResponse_label = Lens.lens (\DescribeLunaClientResponse' {label} -> label) (\s@DescribeLunaClientResponse' {} a -> s {label = a} :: DescribeLunaClientResponse)

-- | The certificate installed on the HSMs used by this client.
describeLunaClientResponse_certificate :: Lens.Lens' DescribeLunaClientResponse (Core.Maybe Core.Text)
describeLunaClientResponse_certificate = Lens.lens (\DescribeLunaClientResponse' {certificate} -> certificate) (\s@DescribeLunaClientResponse' {} a -> s {certificate = a} :: DescribeLunaClientResponse)

-- | The certificate fingerprint.
describeLunaClientResponse_certificateFingerprint :: Lens.Lens' DescribeLunaClientResponse (Core.Maybe Core.Text)
describeLunaClientResponse_certificateFingerprint = Lens.lens (\DescribeLunaClientResponse' {certificateFingerprint} -> certificateFingerprint) (\s@DescribeLunaClientResponse' {} a -> s {certificateFingerprint = a} :: DescribeLunaClientResponse)

-- | The response's http status code.
describeLunaClientResponse_httpStatus :: Lens.Lens' DescribeLunaClientResponse Core.Int
describeLunaClientResponse_httpStatus = Lens.lens (\DescribeLunaClientResponse' {httpStatus} -> httpStatus) (\s@DescribeLunaClientResponse' {} a -> s {httpStatus = a} :: DescribeLunaClientResponse)

instance Core.NFData DescribeLunaClientResponse
