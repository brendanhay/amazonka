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
-- Module      : Amazonka.VoiceId.OptOutSpeaker
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Opts out a speaker from Voice ID. A speaker can be opted out regardless
-- of whether or not they already exist in Voice ID. If they don\'t yet
-- exist, a new speaker is created in an opted out state. If they already
-- exist, their existing status is overridden and they are opted out.
-- Enrollment and evaluation authentication requests are rejected for opted
-- out speakers, and opted out speakers have no voice embeddings stored in
-- Voice ID.
module Amazonka.VoiceId.OptOutSpeaker
  ( -- * Creating a Request
    OptOutSpeaker (..),
    newOptOutSpeaker,

    -- * Request Lenses
    optOutSpeaker_domainId,
    optOutSpeaker_speakerId,

    -- * Destructuring the Response
    OptOutSpeakerResponse (..),
    newOptOutSpeakerResponse,

    -- * Response Lenses
    optOutSpeakerResponse_speaker,
    optOutSpeakerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VoiceId.Types

-- | /See:/ 'newOptOutSpeaker' smart constructor.
data OptOutSpeaker = OptOutSpeaker'
  { -- | The identifier of the domain that contains the speaker.
    domainId :: Prelude.Text,
    -- | The identifier of the speaker you want opted-out.
    speakerId :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OptOutSpeaker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainId', 'optOutSpeaker_domainId' - The identifier of the domain that contains the speaker.
--
-- 'speakerId', 'optOutSpeaker_speakerId' - The identifier of the speaker you want opted-out.
newOptOutSpeaker ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'speakerId'
  Prelude.Text ->
  OptOutSpeaker
newOptOutSpeaker pDomainId_ pSpeakerId_ =
  OptOutSpeaker'
    { domainId = pDomainId_,
      speakerId = Data._Sensitive Lens.# pSpeakerId_
    }

-- | The identifier of the domain that contains the speaker.
optOutSpeaker_domainId :: Lens.Lens' OptOutSpeaker Prelude.Text
optOutSpeaker_domainId = Lens.lens (\OptOutSpeaker' {domainId} -> domainId) (\s@OptOutSpeaker' {} a -> s {domainId = a} :: OptOutSpeaker)

-- | The identifier of the speaker you want opted-out.
optOutSpeaker_speakerId :: Lens.Lens' OptOutSpeaker Prelude.Text
optOutSpeaker_speakerId = Lens.lens (\OptOutSpeaker' {speakerId} -> speakerId) (\s@OptOutSpeaker' {} a -> s {speakerId = a} :: OptOutSpeaker) Prelude.. Data._Sensitive

instance Core.AWSRequest OptOutSpeaker where
  type
    AWSResponse OptOutSpeaker =
      OptOutSpeakerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          OptOutSpeakerResponse'
            Prelude.<$> (x Data..?> "Speaker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable OptOutSpeaker where
  hashWithSalt _salt OptOutSpeaker' {..} =
    _salt
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` speakerId

instance Prelude.NFData OptOutSpeaker where
  rnf OptOutSpeaker' {..} =
    Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf speakerId

instance Data.ToHeaders OptOutSpeaker where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("VoiceID.OptOutSpeaker" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON OptOutSpeaker where
  toJSON OptOutSpeaker' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainId" Data..= domainId),
            Prelude.Just ("SpeakerId" Data..= speakerId)
          ]
      )

instance Data.ToPath OptOutSpeaker where
  toPath = Prelude.const "/"

instance Data.ToQuery OptOutSpeaker where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newOptOutSpeakerResponse' smart constructor.
data OptOutSpeakerResponse = OptOutSpeakerResponse'
  { -- | Details about the opted-out speaker.
    speaker :: Prelude.Maybe Speaker,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OptOutSpeakerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'speaker', 'optOutSpeakerResponse_speaker' - Details about the opted-out speaker.
--
-- 'httpStatus', 'optOutSpeakerResponse_httpStatus' - The response's http status code.
newOptOutSpeakerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  OptOutSpeakerResponse
newOptOutSpeakerResponse pHttpStatus_ =
  OptOutSpeakerResponse'
    { speaker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the opted-out speaker.
optOutSpeakerResponse_speaker :: Lens.Lens' OptOutSpeakerResponse (Prelude.Maybe Speaker)
optOutSpeakerResponse_speaker = Lens.lens (\OptOutSpeakerResponse' {speaker} -> speaker) (\s@OptOutSpeakerResponse' {} a -> s {speaker = a} :: OptOutSpeakerResponse)

-- | The response's http status code.
optOutSpeakerResponse_httpStatus :: Lens.Lens' OptOutSpeakerResponse Prelude.Int
optOutSpeakerResponse_httpStatus = Lens.lens (\OptOutSpeakerResponse' {httpStatus} -> httpStatus) (\s@OptOutSpeakerResponse' {} a -> s {httpStatus = a} :: OptOutSpeakerResponse)

instance Prelude.NFData OptOutSpeakerResponse where
  rnf OptOutSpeakerResponse' {..} =
    Prelude.rnf speaker
      `Prelude.seq` Prelude.rnf httpStatus
