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
-- Module      : Amazonka.Rekognition.CreateFaceLivenessSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API operation initiates a Face Liveness session. It returns a
-- @SessionId@, which you can use to start streaming Face Liveness video
-- and get the results for a Face Liveness session. You can use the
-- @OutputConfig@ option in the Settings parameter to provide an Amazon S3
-- bucket location. The Amazon S3 bucket stores reference images and audit
-- images. You can use @AuditImagesLimit@ to limit the number of audit
-- images returned. This number is between 0 and 4. By default, it is set
-- to 0. The limit is best effort and based on the duration of the
-- selfie-video.
module Amazonka.Rekognition.CreateFaceLivenessSession
  ( -- * Creating a Request
    CreateFaceLivenessSession (..),
    newCreateFaceLivenessSession,

    -- * Request Lenses
    createFaceLivenessSession_clientRequestToken,
    createFaceLivenessSession_kmsKeyId,
    createFaceLivenessSession_settings,

    -- * Destructuring the Response
    CreateFaceLivenessSessionResponse (..),
    newCreateFaceLivenessSessionResponse,

    -- * Response Lenses
    createFaceLivenessSessionResponse_httpStatus,
    createFaceLivenessSessionResponse_sessionId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFaceLivenessSession' smart constructor.
data CreateFaceLivenessSession = CreateFaceLivenessSession'
  { -- | Idempotent token is used to recognize the Face Liveness request. If the
    -- same token is used with multiple @CreateFaceLivenessSession@ requests,
    -- the same session is returned. This token is employed to avoid
    -- unintentionally creating the same session multiple times.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier for your AWS Key Management Service key (AWS KMS key).
    -- Used to encrypt audit images and reference images.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A session settings object. It contains settings for the operation to be
    -- performed. For Face Liveness, it accepts @OutputConfig@ and
    -- @AuditImagesLimit@.
    settings :: Prelude.Maybe CreateFaceLivenessSessionRequestSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFaceLivenessSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createFaceLivenessSession_clientRequestToken' - Idempotent token is used to recognize the Face Liveness request. If the
-- same token is used with multiple @CreateFaceLivenessSession@ requests,
-- the same session is returned. This token is employed to avoid
-- unintentionally creating the same session multiple times.
--
-- 'kmsKeyId', 'createFaceLivenessSession_kmsKeyId' - The identifier for your AWS Key Management Service key (AWS KMS key).
-- Used to encrypt audit images and reference images.
--
-- 'settings', 'createFaceLivenessSession_settings' - A session settings object. It contains settings for the operation to be
-- performed. For Face Liveness, it accepts @OutputConfig@ and
-- @AuditImagesLimit@.
newCreateFaceLivenessSession ::
  CreateFaceLivenessSession
newCreateFaceLivenessSession =
  CreateFaceLivenessSession'
    { clientRequestToken =
        Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      settings = Prelude.Nothing
    }

-- | Idempotent token is used to recognize the Face Liveness request. If the
-- same token is used with multiple @CreateFaceLivenessSession@ requests,
-- the same session is returned. This token is employed to avoid
-- unintentionally creating the same session multiple times.
createFaceLivenessSession_clientRequestToken :: Lens.Lens' CreateFaceLivenessSession (Prelude.Maybe Prelude.Text)
createFaceLivenessSession_clientRequestToken = Lens.lens (\CreateFaceLivenessSession' {clientRequestToken} -> clientRequestToken) (\s@CreateFaceLivenessSession' {} a -> s {clientRequestToken = a} :: CreateFaceLivenessSession)

-- | The identifier for your AWS Key Management Service key (AWS KMS key).
-- Used to encrypt audit images and reference images.
createFaceLivenessSession_kmsKeyId :: Lens.Lens' CreateFaceLivenessSession (Prelude.Maybe Prelude.Text)
createFaceLivenessSession_kmsKeyId = Lens.lens (\CreateFaceLivenessSession' {kmsKeyId} -> kmsKeyId) (\s@CreateFaceLivenessSession' {} a -> s {kmsKeyId = a} :: CreateFaceLivenessSession)

-- | A session settings object. It contains settings for the operation to be
-- performed. For Face Liveness, it accepts @OutputConfig@ and
-- @AuditImagesLimit@.
createFaceLivenessSession_settings :: Lens.Lens' CreateFaceLivenessSession (Prelude.Maybe CreateFaceLivenessSessionRequestSettings)
createFaceLivenessSession_settings = Lens.lens (\CreateFaceLivenessSession' {settings} -> settings) (\s@CreateFaceLivenessSession' {} a -> s {settings = a} :: CreateFaceLivenessSession)

instance Core.AWSRequest CreateFaceLivenessSession where
  type
    AWSResponse CreateFaceLivenessSession =
      CreateFaceLivenessSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFaceLivenessSessionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "SessionId")
      )

instance Prelude.Hashable CreateFaceLivenessSession where
  hashWithSalt _salt CreateFaceLivenessSession' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` settings

instance Prelude.NFData CreateFaceLivenessSession where
  rnf CreateFaceLivenessSession' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf settings

instance Data.ToHeaders CreateFaceLivenessSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.CreateFaceLivenessSession" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateFaceLivenessSession where
  toJSON CreateFaceLivenessSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("Settings" Data..=) Prelude.<$> settings
          ]
      )

instance Data.ToPath CreateFaceLivenessSession where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateFaceLivenessSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFaceLivenessSessionResponse' smart constructor.
data CreateFaceLivenessSessionResponse = CreateFaceLivenessSessionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A unique 128-bit UUID identifying a Face Liveness session.
    sessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFaceLivenessSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createFaceLivenessSessionResponse_httpStatus' - The response's http status code.
--
-- 'sessionId', 'createFaceLivenessSessionResponse_sessionId' - A unique 128-bit UUID identifying a Face Liveness session.
newCreateFaceLivenessSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'sessionId'
  Prelude.Text ->
  CreateFaceLivenessSessionResponse
newCreateFaceLivenessSessionResponse
  pHttpStatus_
  pSessionId_ =
    CreateFaceLivenessSessionResponse'
      { httpStatus =
          pHttpStatus_,
        sessionId = pSessionId_
      }

-- | The response's http status code.
createFaceLivenessSessionResponse_httpStatus :: Lens.Lens' CreateFaceLivenessSessionResponse Prelude.Int
createFaceLivenessSessionResponse_httpStatus = Lens.lens (\CreateFaceLivenessSessionResponse' {httpStatus} -> httpStatus) (\s@CreateFaceLivenessSessionResponse' {} a -> s {httpStatus = a} :: CreateFaceLivenessSessionResponse)

-- | A unique 128-bit UUID identifying a Face Liveness session.
createFaceLivenessSessionResponse_sessionId :: Lens.Lens' CreateFaceLivenessSessionResponse Prelude.Text
createFaceLivenessSessionResponse_sessionId = Lens.lens (\CreateFaceLivenessSessionResponse' {sessionId} -> sessionId) (\s@CreateFaceLivenessSessionResponse' {} a -> s {sessionId = a} :: CreateFaceLivenessSessionResponse)

instance
  Prelude.NFData
    CreateFaceLivenessSessionResponse
  where
  rnf CreateFaceLivenessSessionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf sessionId
