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
-- Module      : Amazonka.Nimble.CreateStreamingSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a streaming session in a studio.
--
-- After invoking this operation, you must poll GetStreamingSession until
-- the streaming session is in the @READY@ state.
module Amazonka.Nimble.CreateStreamingSession
  ( -- * Creating a Request
    CreateStreamingSession (..),
    newCreateStreamingSession,

    -- * Request Lenses
    createStreamingSession_clientToken,
    createStreamingSession_ec2InstanceType,
    createStreamingSession_ownedBy,
    createStreamingSession_streamingImageId,
    createStreamingSession_tags,
    createStreamingSession_launchProfileId,
    createStreamingSession_studioId,

    -- * Destructuring the Response
    CreateStreamingSessionResponse (..),
    newCreateStreamingSessionResponse,

    -- * Response Lenses
    createStreamingSessionResponse_session,
    createStreamingSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateStreamingSession' smart constructor.
data CreateStreamingSession = CreateStreamingSession'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you don’t specify a client token, the
    -- Amazon Web Services SDK automatically generates a client token and uses
    -- it for the request to ensure idempotency.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The EC2 Instance type used for the streaming session.
    ec2InstanceType :: Prelude.Maybe StreamingInstanceType,
    -- | The user ID of the user that owns the streaming session. The user that
    -- owns the session will be logging into the session and interacting with
    -- the virtual workstation.
    ownedBy :: Prelude.Maybe Prelude.Text,
    -- | The ID of the streaming image.
    streamingImageId :: Prelude.Maybe Prelude.Text,
    -- | A collection of labels, in the form of key-value pairs, that apply to
    -- this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID of the launch profile used to control access from the streaming
    -- session.
    launchProfileId :: Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStreamingSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createStreamingSession_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the
-- Amazon Web Services SDK automatically generates a client token and uses
-- it for the request to ensure idempotency.
--
-- 'ec2InstanceType', 'createStreamingSession_ec2InstanceType' - The EC2 Instance type used for the streaming session.
--
-- 'ownedBy', 'createStreamingSession_ownedBy' - The user ID of the user that owns the streaming session. The user that
-- owns the session will be logging into the session and interacting with
-- the virtual workstation.
--
-- 'streamingImageId', 'createStreamingSession_streamingImageId' - The ID of the streaming image.
--
-- 'tags', 'createStreamingSession_tags' - A collection of labels, in the form of key-value pairs, that apply to
-- this resource.
--
-- 'launchProfileId', 'createStreamingSession_launchProfileId' - The ID of the launch profile used to control access from the streaming
-- session.
--
-- 'studioId', 'createStreamingSession_studioId' - The studio ID.
newCreateStreamingSession ::
  -- | 'launchProfileId'
  Prelude.Text ->
  -- | 'studioId'
  Prelude.Text ->
  CreateStreamingSession
newCreateStreamingSession
  pLaunchProfileId_
  pStudioId_ =
    CreateStreamingSession'
      { clientToken =
          Prelude.Nothing,
        ec2InstanceType = Prelude.Nothing,
        ownedBy = Prelude.Nothing,
        streamingImageId = Prelude.Nothing,
        tags = Prelude.Nothing,
        launchProfileId = pLaunchProfileId_,
        studioId = pStudioId_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you don’t specify a client token, the
-- Amazon Web Services SDK automatically generates a client token and uses
-- it for the request to ensure idempotency.
createStreamingSession_clientToken :: Lens.Lens' CreateStreamingSession (Prelude.Maybe Prelude.Text)
createStreamingSession_clientToken = Lens.lens (\CreateStreamingSession' {clientToken} -> clientToken) (\s@CreateStreamingSession' {} a -> s {clientToken = a} :: CreateStreamingSession)

-- | The EC2 Instance type used for the streaming session.
createStreamingSession_ec2InstanceType :: Lens.Lens' CreateStreamingSession (Prelude.Maybe StreamingInstanceType)
createStreamingSession_ec2InstanceType = Lens.lens (\CreateStreamingSession' {ec2InstanceType} -> ec2InstanceType) (\s@CreateStreamingSession' {} a -> s {ec2InstanceType = a} :: CreateStreamingSession)

-- | The user ID of the user that owns the streaming session. The user that
-- owns the session will be logging into the session and interacting with
-- the virtual workstation.
createStreamingSession_ownedBy :: Lens.Lens' CreateStreamingSession (Prelude.Maybe Prelude.Text)
createStreamingSession_ownedBy = Lens.lens (\CreateStreamingSession' {ownedBy} -> ownedBy) (\s@CreateStreamingSession' {} a -> s {ownedBy = a} :: CreateStreamingSession)

-- | The ID of the streaming image.
createStreamingSession_streamingImageId :: Lens.Lens' CreateStreamingSession (Prelude.Maybe Prelude.Text)
createStreamingSession_streamingImageId = Lens.lens (\CreateStreamingSession' {streamingImageId} -> streamingImageId) (\s@CreateStreamingSession' {} a -> s {streamingImageId = a} :: CreateStreamingSession)

-- | A collection of labels, in the form of key-value pairs, that apply to
-- this resource.
createStreamingSession_tags :: Lens.Lens' CreateStreamingSession (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createStreamingSession_tags = Lens.lens (\CreateStreamingSession' {tags} -> tags) (\s@CreateStreamingSession' {} a -> s {tags = a} :: CreateStreamingSession) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the launch profile used to control access from the streaming
-- session.
createStreamingSession_launchProfileId :: Lens.Lens' CreateStreamingSession Prelude.Text
createStreamingSession_launchProfileId = Lens.lens (\CreateStreamingSession' {launchProfileId} -> launchProfileId) (\s@CreateStreamingSession' {} a -> s {launchProfileId = a} :: CreateStreamingSession)

-- | The studio ID.
createStreamingSession_studioId :: Lens.Lens' CreateStreamingSession Prelude.Text
createStreamingSession_studioId = Lens.lens (\CreateStreamingSession' {studioId} -> studioId) (\s@CreateStreamingSession' {} a -> s {studioId = a} :: CreateStreamingSession)

instance Core.AWSRequest CreateStreamingSession where
  type
    AWSResponse CreateStreamingSession =
      CreateStreamingSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStreamingSessionResponse'
            Prelude.<$> (x Data..?> "session")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStreamingSession where
  hashWithSalt _salt CreateStreamingSession' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` ec2InstanceType
      `Prelude.hashWithSalt` ownedBy
      `Prelude.hashWithSalt` streamingImageId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` launchProfileId
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData CreateStreamingSession where
  rnf CreateStreamingSession' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf ec2InstanceType
      `Prelude.seq` Prelude.rnf ownedBy
      `Prelude.seq` Prelude.rnf streamingImageId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf launchProfileId
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders CreateStreamingSession where
  toHeaders CreateStreamingSession' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Data.=# clientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON CreateStreamingSession where
  toJSON CreateStreamingSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ec2InstanceType" Data..=)
              Prelude.<$> ec2InstanceType,
            ("ownedBy" Data..=) Prelude.<$> ownedBy,
            ("streamingImageId" Data..=)
              Prelude.<$> streamingImageId,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("launchProfileId" Data..= launchProfileId)
          ]
      )

instance Data.ToPath CreateStreamingSession where
  toPath CreateStreamingSession' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/streaming-sessions"
      ]

instance Data.ToQuery CreateStreamingSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStreamingSessionResponse' smart constructor.
data CreateStreamingSessionResponse = CreateStreamingSessionResponse'
  { -- | The session.
    session :: Prelude.Maybe StreamingSession,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStreamingSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'session', 'createStreamingSessionResponse_session' - The session.
--
-- 'httpStatus', 'createStreamingSessionResponse_httpStatus' - The response's http status code.
newCreateStreamingSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStreamingSessionResponse
newCreateStreamingSessionResponse pHttpStatus_ =
  CreateStreamingSessionResponse'
    { session =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The session.
createStreamingSessionResponse_session :: Lens.Lens' CreateStreamingSessionResponse (Prelude.Maybe StreamingSession)
createStreamingSessionResponse_session = Lens.lens (\CreateStreamingSessionResponse' {session} -> session) (\s@CreateStreamingSessionResponse' {} a -> s {session = a} :: CreateStreamingSessionResponse)

-- | The response's http status code.
createStreamingSessionResponse_httpStatus :: Lens.Lens' CreateStreamingSessionResponse Prelude.Int
createStreamingSessionResponse_httpStatus = Lens.lens (\CreateStreamingSessionResponse' {httpStatus} -> httpStatus) (\s@CreateStreamingSessionResponse' {} a -> s {httpStatus = a} :: CreateStreamingSessionResponse)

instance
  Prelude.NFData
    CreateStreamingSessionResponse
  where
  rnf CreateStreamingSessionResponse' {..} =
    Prelude.rnf session
      `Prelude.seq` Prelude.rnf httpStatus
