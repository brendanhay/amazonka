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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a streaming session in a studio.
--
-- After invoking this operation, you must poll GetStreamingSession until
-- the streaming session is in state READY.
module Amazonka.Nimble.CreateStreamingSession
  ( -- * Creating a Request
    CreateStreamingSession (..),
    newCreateStreamingSession,

    -- * Request Lenses
    createStreamingSession_ownedBy,
    createStreamingSession_clientToken,
    createStreamingSession_ec2InstanceType,
    createStreamingSession_launchProfileId,
    createStreamingSession_streamingImageId,
    createStreamingSession_tags,
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
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A collection of streaming sessions.
--
-- /See:/ 'newCreateStreamingSession' smart constructor.
data CreateStreamingSession = CreateStreamingSession'
  { -- | The user ID of the user that owns the streaming session.
    ownedBy :: Prelude.Maybe Prelude.Text,
    -- | To make an idempotent API request using one of these actions, specify a
    -- client token in the request. You should not reuse the same client token
    -- for other API requests. If you retry a request that completed
    -- successfully using the same client token and the same parameters, the
    -- retry succeeds without performing any further actions. If you retry a
    -- successful request using the same client token, but one or more of the
    -- parameters are different, the retry fails with a ValidationException
    -- error.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The EC2 Instance type used for the streaming session.
    ec2InstanceType :: Prelude.Maybe StreamingInstanceType,
    -- | The launch profile ID.
    launchProfileId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the streaming image.
    streamingImageId :: Prelude.Maybe Prelude.Text,
    -- | A collection of labels, in the form of key:value pairs, that apply to
    -- this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'ownedBy', 'createStreamingSession_ownedBy' - The user ID of the user that owns the streaming session.
--
-- 'clientToken', 'createStreamingSession_clientToken' - To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
--
-- 'ec2InstanceType', 'createStreamingSession_ec2InstanceType' - The EC2 Instance type used for the streaming session.
--
-- 'launchProfileId', 'createStreamingSession_launchProfileId' - The launch profile ID.
--
-- 'streamingImageId', 'createStreamingSession_streamingImageId' - The ID of the streaming image.
--
-- 'tags', 'createStreamingSession_tags' - A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
--
-- 'studioId', 'createStreamingSession_studioId' - The studio ID.
newCreateStreamingSession ::
  -- | 'studioId'
  Prelude.Text ->
  CreateStreamingSession
newCreateStreamingSession pStudioId_ =
  CreateStreamingSession'
    { ownedBy = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      ec2InstanceType = Prelude.Nothing,
      launchProfileId = Prelude.Nothing,
      streamingImageId = Prelude.Nothing,
      tags = Prelude.Nothing,
      studioId = pStudioId_
    }

-- | The user ID of the user that owns the streaming session.
createStreamingSession_ownedBy :: Lens.Lens' CreateStreamingSession (Prelude.Maybe Prelude.Text)
createStreamingSession_ownedBy = Lens.lens (\CreateStreamingSession' {ownedBy} -> ownedBy) (\s@CreateStreamingSession' {} a -> s {ownedBy = a} :: CreateStreamingSession)

-- | To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
createStreamingSession_clientToken :: Lens.Lens' CreateStreamingSession (Prelude.Maybe Prelude.Text)
createStreamingSession_clientToken = Lens.lens (\CreateStreamingSession' {clientToken} -> clientToken) (\s@CreateStreamingSession' {} a -> s {clientToken = a} :: CreateStreamingSession)

-- | The EC2 Instance type used for the streaming session.
createStreamingSession_ec2InstanceType :: Lens.Lens' CreateStreamingSession (Prelude.Maybe StreamingInstanceType)
createStreamingSession_ec2InstanceType = Lens.lens (\CreateStreamingSession' {ec2InstanceType} -> ec2InstanceType) (\s@CreateStreamingSession' {} a -> s {ec2InstanceType = a} :: CreateStreamingSession)

-- | The launch profile ID.
createStreamingSession_launchProfileId :: Lens.Lens' CreateStreamingSession (Prelude.Maybe Prelude.Text)
createStreamingSession_launchProfileId = Lens.lens (\CreateStreamingSession' {launchProfileId} -> launchProfileId) (\s@CreateStreamingSession' {} a -> s {launchProfileId = a} :: CreateStreamingSession)

-- | The ID of the streaming image.
createStreamingSession_streamingImageId :: Lens.Lens' CreateStreamingSession (Prelude.Maybe Prelude.Text)
createStreamingSession_streamingImageId = Lens.lens (\CreateStreamingSession' {streamingImageId} -> streamingImageId) (\s@CreateStreamingSession' {} a -> s {streamingImageId = a} :: CreateStreamingSession)

-- | A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
createStreamingSession_tags :: Lens.Lens' CreateStreamingSession (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createStreamingSession_tags = Lens.lens (\CreateStreamingSession' {tags} -> tags) (\s@CreateStreamingSession' {} a -> s {tags = a} :: CreateStreamingSession) Prelude.. Lens.mapping Lens.coerced

-- | The studio ID.
createStreamingSession_studioId :: Lens.Lens' CreateStreamingSession Prelude.Text
createStreamingSession_studioId = Lens.lens (\CreateStreamingSession' {studioId} -> studioId) (\s@CreateStreamingSession' {} a -> s {studioId = a} :: CreateStreamingSession)

instance Core.AWSRequest CreateStreamingSession where
  type
    AWSResponse CreateStreamingSession =
      CreateStreamingSessionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStreamingSessionResponse'
            Prelude.<$> (x Core..?> "session")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStreamingSession where
  hashWithSalt salt' CreateStreamingSession' {..} =
    salt' `Prelude.hashWithSalt` studioId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` streamingImageId
      `Prelude.hashWithSalt` launchProfileId
      `Prelude.hashWithSalt` ec2InstanceType
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` ownedBy

instance Prelude.NFData CreateStreamingSession where
  rnf CreateStreamingSession' {..} =
    Prelude.rnf ownedBy
      `Prelude.seq` Prelude.rnf studioId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf streamingImageId
      `Prelude.seq` Prelude.rnf launchProfileId
      `Prelude.seq` Prelude.rnf ec2InstanceType
      `Prelude.seq` Prelude.rnf clientToken

instance Core.ToHeaders CreateStreamingSession where
  toHeaders CreateStreamingSession' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Core.=# clientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON CreateStreamingSession where
  toJSON CreateStreamingSession' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ownedBy" Core..=) Prelude.<$> ownedBy,
            ("ec2InstanceType" Core..=)
              Prelude.<$> ec2InstanceType,
            ("launchProfileId" Core..=)
              Prelude.<$> launchProfileId,
            ("streamingImageId" Core..=)
              Prelude.<$> streamingImageId,
            ("tags" Core..=) Prelude.<$> tags
          ]
      )

instance Core.ToPath CreateStreamingSession where
  toPath CreateStreamingSession' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/streaming-sessions"
      ]

instance Core.ToQuery CreateStreamingSession where
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
