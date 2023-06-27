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
-- Module      : Amazonka.Signer.SignPayload
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Signs a binary payload and returns a signature envelope.
module Amazonka.Signer.SignPayload
  ( -- * Creating a Request
    SignPayload (..),
    newSignPayload,

    -- * Request Lenses
    signPayload_profileOwner,
    signPayload_profileName,
    signPayload_payload,
    signPayload_payloadFormat,

    -- * Destructuring the Response
    SignPayloadResponse (..),
    newSignPayloadResponse,

    -- * Response Lenses
    signPayloadResponse_jobId,
    signPayloadResponse_jobOwner,
    signPayloadResponse_metadata,
    signPayloadResponse_signature,
    signPayloadResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Signer.Types

-- | /See:/ 'newSignPayload' smart constructor.
data SignPayload = SignPayload'
  { -- | The AWS account ID of the profile owner.
    profileOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the signing profile.
    profileName :: Prelude.Text,
    -- | Specifies the object digest (hash) to sign.
    payload :: Data.Base64,
    -- | Payload content type
    payloadFormat :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SignPayload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileOwner', 'signPayload_profileOwner' - The AWS account ID of the profile owner.
--
-- 'profileName', 'signPayload_profileName' - The name of the signing profile.
--
-- 'payload', 'signPayload_payload' - Specifies the object digest (hash) to sign.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'payloadFormat', 'signPayload_payloadFormat' - Payload content type
newSignPayload ::
  -- | 'profileName'
  Prelude.Text ->
  -- | 'payload'
  Prelude.ByteString ->
  -- | 'payloadFormat'
  Prelude.Text ->
  SignPayload
newSignPayload
  pProfileName_
  pPayload_
  pPayloadFormat_ =
    SignPayload'
      { profileOwner = Prelude.Nothing,
        profileName = pProfileName_,
        payload = Data._Base64 Lens.# pPayload_,
        payloadFormat = pPayloadFormat_
      }

-- | The AWS account ID of the profile owner.
signPayload_profileOwner :: Lens.Lens' SignPayload (Prelude.Maybe Prelude.Text)
signPayload_profileOwner = Lens.lens (\SignPayload' {profileOwner} -> profileOwner) (\s@SignPayload' {} a -> s {profileOwner = a} :: SignPayload)

-- | The name of the signing profile.
signPayload_profileName :: Lens.Lens' SignPayload Prelude.Text
signPayload_profileName = Lens.lens (\SignPayload' {profileName} -> profileName) (\s@SignPayload' {} a -> s {profileName = a} :: SignPayload)

-- | Specifies the object digest (hash) to sign.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
signPayload_payload :: Lens.Lens' SignPayload Prelude.ByteString
signPayload_payload = Lens.lens (\SignPayload' {payload} -> payload) (\s@SignPayload' {} a -> s {payload = a} :: SignPayload) Prelude.. Data._Base64

-- | Payload content type
signPayload_payloadFormat :: Lens.Lens' SignPayload Prelude.Text
signPayload_payloadFormat = Lens.lens (\SignPayload' {payloadFormat} -> payloadFormat) (\s@SignPayload' {} a -> s {payloadFormat = a} :: SignPayload)

instance Core.AWSRequest SignPayload where
  type AWSResponse SignPayload = SignPayloadResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SignPayloadResponse'
            Prelude.<$> (x Data..?> "jobId")
            Prelude.<*> (x Data..?> "jobOwner")
            Prelude.<*> (x Data..?> "metadata" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "signature")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SignPayload where
  hashWithSalt _salt SignPayload' {..} =
    _salt
      `Prelude.hashWithSalt` profileOwner
      `Prelude.hashWithSalt` profileName
      `Prelude.hashWithSalt` payload
      `Prelude.hashWithSalt` payloadFormat

instance Prelude.NFData SignPayload where
  rnf SignPayload' {..} =
    Prelude.rnf profileOwner
      `Prelude.seq` Prelude.rnf profileName
      `Prelude.seq` Prelude.rnf payload
      `Prelude.seq` Prelude.rnf payloadFormat

instance Data.ToHeaders SignPayload where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SignPayload where
  toJSON SignPayload' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("profileOwner" Data..=) Prelude.<$> profileOwner,
            Prelude.Just ("profileName" Data..= profileName),
            Prelude.Just ("payload" Data..= payload),
            Prelude.Just
              ("payloadFormat" Data..= payloadFormat)
          ]
      )

instance Data.ToPath SignPayload where
  toPath = Prelude.const "/signing-jobs/with-payload"

instance Data.ToQuery SignPayload where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSignPayloadResponse' smart constructor.
data SignPayloadResponse = SignPayloadResponse'
  { -- | Unique identifier of the signing job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The AWS account ID of the job owner.
    jobOwner :: Prelude.Maybe Prelude.Text,
    -- | Information including the signing profile ARN and the signing job ID.
    -- Clients use metadata to signature records, for example, as annotations
    -- added to the signature manifest inside an OCI registry.
    metadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A cryptographic signature.
    signature :: Prelude.Maybe Data.Base64,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SignPayloadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'signPayloadResponse_jobId' - Unique identifier of the signing job.
--
-- 'jobOwner', 'signPayloadResponse_jobOwner' - The AWS account ID of the job owner.
--
-- 'metadata', 'signPayloadResponse_metadata' - Information including the signing profile ARN and the signing job ID.
-- Clients use metadata to signature records, for example, as annotations
-- added to the signature manifest inside an OCI registry.
--
-- 'signature', 'signPayloadResponse_signature' - A cryptographic signature.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'httpStatus', 'signPayloadResponse_httpStatus' - The response's http status code.
newSignPayloadResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SignPayloadResponse
newSignPayloadResponse pHttpStatus_ =
  SignPayloadResponse'
    { jobId = Prelude.Nothing,
      jobOwner = Prelude.Nothing,
      metadata = Prelude.Nothing,
      signature = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Unique identifier of the signing job.
signPayloadResponse_jobId :: Lens.Lens' SignPayloadResponse (Prelude.Maybe Prelude.Text)
signPayloadResponse_jobId = Lens.lens (\SignPayloadResponse' {jobId} -> jobId) (\s@SignPayloadResponse' {} a -> s {jobId = a} :: SignPayloadResponse)

-- | The AWS account ID of the job owner.
signPayloadResponse_jobOwner :: Lens.Lens' SignPayloadResponse (Prelude.Maybe Prelude.Text)
signPayloadResponse_jobOwner = Lens.lens (\SignPayloadResponse' {jobOwner} -> jobOwner) (\s@SignPayloadResponse' {} a -> s {jobOwner = a} :: SignPayloadResponse)

-- | Information including the signing profile ARN and the signing job ID.
-- Clients use metadata to signature records, for example, as annotations
-- added to the signature manifest inside an OCI registry.
signPayloadResponse_metadata :: Lens.Lens' SignPayloadResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
signPayloadResponse_metadata = Lens.lens (\SignPayloadResponse' {metadata} -> metadata) (\s@SignPayloadResponse' {} a -> s {metadata = a} :: SignPayloadResponse) Prelude.. Lens.mapping Lens.coerced

-- | A cryptographic signature.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
signPayloadResponse_signature :: Lens.Lens' SignPayloadResponse (Prelude.Maybe Prelude.ByteString)
signPayloadResponse_signature = Lens.lens (\SignPayloadResponse' {signature} -> signature) (\s@SignPayloadResponse' {} a -> s {signature = a} :: SignPayloadResponse) Prelude.. Lens.mapping Data._Base64

-- | The response's http status code.
signPayloadResponse_httpStatus :: Lens.Lens' SignPayloadResponse Prelude.Int
signPayloadResponse_httpStatus = Lens.lens (\SignPayloadResponse' {httpStatus} -> httpStatus) (\s@SignPayloadResponse' {} a -> s {httpStatus = a} :: SignPayloadResponse)

instance Prelude.NFData SignPayloadResponse where
  rnf SignPayloadResponse' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobOwner
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf signature
      `Prelude.seq` Prelude.rnf httpStatus
