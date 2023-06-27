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
-- Module      : Amazonka.IVSRealtime.CreateParticipantToken
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an additional token for a specified stage. This can be done
-- after stage creation or when tokens expire. Tokens always are scoped to
-- the stage for which they are created.
--
-- Encryption keys are owned by Amazon IVS and never used directly by your
-- application.
module Amazonka.IVSRealtime.CreateParticipantToken
  ( -- * Creating a Request
    CreateParticipantToken (..),
    newCreateParticipantToken,

    -- * Request Lenses
    createParticipantToken_attributes,
    createParticipantToken_capabilities,
    createParticipantToken_duration,
    createParticipantToken_userId,
    createParticipantToken_stageArn,

    -- * Destructuring the Response
    CreateParticipantTokenResponse (..),
    newCreateParticipantTokenResponse,

    -- * Response Lenses
    createParticipantTokenResponse_participantToken,
    createParticipantTokenResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSRealtime.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateParticipantToken' smart constructor.
data CreateParticipantToken = CreateParticipantToken'
  { -- | Application-provided attributes to encode into the token and attach to a
    -- stage. Map keys and values can contain UTF-8 encoded text. The maximum
    -- length of this field is 1 KB total. /This field is exposed to all stage
    -- participants and should not be used for personally identifying,
    -- confidential, or sensitive information./
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Set of capabilities that the user is allowed to perform in the stage.
    -- Default: @PUBLISH, SUBSCRIBE@.
    capabilities :: Prelude.Maybe [ParticipantTokenCapability],
    -- | Duration (in minutes), after which the token expires. Default: 720 (12
    -- hours).
    duration :: Prelude.Maybe Prelude.Natural,
    -- | Name that can be specified to help identify the token. This can be any
    -- UTF-8 encoded text. /This field is exposed to all stage participants and
    -- should not be used for personally identifying, confidential, or
    -- sensitive information./
    userId :: Prelude.Maybe Prelude.Text,
    -- | ARN of the stage to which this token is scoped.
    stageArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateParticipantToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'createParticipantToken_attributes' - Application-provided attributes to encode into the token and attach to a
-- stage. Map keys and values can contain UTF-8 encoded text. The maximum
-- length of this field is 1 KB total. /This field is exposed to all stage
-- participants and should not be used for personally identifying,
-- confidential, or sensitive information./
--
-- 'capabilities', 'createParticipantToken_capabilities' - Set of capabilities that the user is allowed to perform in the stage.
-- Default: @PUBLISH, SUBSCRIBE@.
--
-- 'duration', 'createParticipantToken_duration' - Duration (in minutes), after which the token expires. Default: 720 (12
-- hours).
--
-- 'userId', 'createParticipantToken_userId' - Name that can be specified to help identify the token. This can be any
-- UTF-8 encoded text. /This field is exposed to all stage participants and
-- should not be used for personally identifying, confidential, or
-- sensitive information./
--
-- 'stageArn', 'createParticipantToken_stageArn' - ARN of the stage to which this token is scoped.
newCreateParticipantToken ::
  -- | 'stageArn'
  Prelude.Text ->
  CreateParticipantToken
newCreateParticipantToken pStageArn_ =
  CreateParticipantToken'
    { attributes =
        Prelude.Nothing,
      capabilities = Prelude.Nothing,
      duration = Prelude.Nothing,
      userId = Prelude.Nothing,
      stageArn = pStageArn_
    }

-- | Application-provided attributes to encode into the token and attach to a
-- stage. Map keys and values can contain UTF-8 encoded text. The maximum
-- length of this field is 1 KB total. /This field is exposed to all stage
-- participants and should not be used for personally identifying,
-- confidential, or sensitive information./
createParticipantToken_attributes :: Lens.Lens' CreateParticipantToken (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createParticipantToken_attributes = Lens.lens (\CreateParticipantToken' {attributes} -> attributes) (\s@CreateParticipantToken' {} a -> s {attributes = a} :: CreateParticipantToken) Prelude.. Lens.mapping Lens.coerced

-- | Set of capabilities that the user is allowed to perform in the stage.
-- Default: @PUBLISH, SUBSCRIBE@.
createParticipantToken_capabilities :: Lens.Lens' CreateParticipantToken (Prelude.Maybe [ParticipantTokenCapability])
createParticipantToken_capabilities = Lens.lens (\CreateParticipantToken' {capabilities} -> capabilities) (\s@CreateParticipantToken' {} a -> s {capabilities = a} :: CreateParticipantToken) Prelude.. Lens.mapping Lens.coerced

-- | Duration (in minutes), after which the token expires. Default: 720 (12
-- hours).
createParticipantToken_duration :: Lens.Lens' CreateParticipantToken (Prelude.Maybe Prelude.Natural)
createParticipantToken_duration = Lens.lens (\CreateParticipantToken' {duration} -> duration) (\s@CreateParticipantToken' {} a -> s {duration = a} :: CreateParticipantToken)

-- | Name that can be specified to help identify the token. This can be any
-- UTF-8 encoded text. /This field is exposed to all stage participants and
-- should not be used for personally identifying, confidential, or
-- sensitive information./
createParticipantToken_userId :: Lens.Lens' CreateParticipantToken (Prelude.Maybe Prelude.Text)
createParticipantToken_userId = Lens.lens (\CreateParticipantToken' {userId} -> userId) (\s@CreateParticipantToken' {} a -> s {userId = a} :: CreateParticipantToken)

-- | ARN of the stage to which this token is scoped.
createParticipantToken_stageArn :: Lens.Lens' CreateParticipantToken Prelude.Text
createParticipantToken_stageArn = Lens.lens (\CreateParticipantToken' {stageArn} -> stageArn) (\s@CreateParticipantToken' {} a -> s {stageArn = a} :: CreateParticipantToken)

instance Core.AWSRequest CreateParticipantToken where
  type
    AWSResponse CreateParticipantToken =
      CreateParticipantTokenResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateParticipantTokenResponse'
            Prelude.<$> (x Data..?> "participantToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateParticipantToken where
  hashWithSalt _salt CreateParticipantToken' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` capabilities
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` stageArn

instance Prelude.NFData CreateParticipantToken where
  rnf CreateParticipantToken' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf capabilities
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf stageArn

instance Data.ToHeaders CreateParticipantToken where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateParticipantToken where
  toJSON CreateParticipantToken' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attributes" Data..=) Prelude.<$> attributes,
            ("capabilities" Data..=) Prelude.<$> capabilities,
            ("duration" Data..=) Prelude.<$> duration,
            ("userId" Data..=) Prelude.<$> userId,
            Prelude.Just ("stageArn" Data..= stageArn)
          ]
      )

instance Data.ToPath CreateParticipantToken where
  toPath = Prelude.const "/CreateParticipantToken"

instance Data.ToQuery CreateParticipantToken where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateParticipantTokenResponse' smart constructor.
data CreateParticipantTokenResponse = CreateParticipantTokenResponse'
  { -- | The participant token that was created.
    participantToken :: Prelude.Maybe ParticipantToken,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateParticipantTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'participantToken', 'createParticipantTokenResponse_participantToken' - The participant token that was created.
--
-- 'httpStatus', 'createParticipantTokenResponse_httpStatus' - The response's http status code.
newCreateParticipantTokenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateParticipantTokenResponse
newCreateParticipantTokenResponse pHttpStatus_ =
  CreateParticipantTokenResponse'
    { participantToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The participant token that was created.
createParticipantTokenResponse_participantToken :: Lens.Lens' CreateParticipantTokenResponse (Prelude.Maybe ParticipantToken)
createParticipantTokenResponse_participantToken = Lens.lens (\CreateParticipantTokenResponse' {participantToken} -> participantToken) (\s@CreateParticipantTokenResponse' {} a -> s {participantToken = a} :: CreateParticipantTokenResponse)

-- | The response's http status code.
createParticipantTokenResponse_httpStatus :: Lens.Lens' CreateParticipantTokenResponse Prelude.Int
createParticipantTokenResponse_httpStatus = Lens.lens (\CreateParticipantTokenResponse' {httpStatus} -> httpStatus) (\s@CreateParticipantTokenResponse' {} a -> s {httpStatus = a} :: CreateParticipantTokenResponse)

instance
  Prelude.NFData
    CreateParticipantTokenResponse
  where
  rnf CreateParticipantTokenResponse' {..} =
    Prelude.rnf participantToken
      `Prelude.seq` Prelude.rnf httpStatus
