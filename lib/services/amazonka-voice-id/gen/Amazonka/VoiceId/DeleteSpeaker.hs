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
-- Module      : Amazonka.VoiceId.DeleteSpeaker
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified speaker from Voice ID.
module Amazonka.VoiceId.DeleteSpeaker
  ( -- * Creating a Request
    DeleteSpeaker (..),
    newDeleteSpeaker,

    -- * Request Lenses
    deleteSpeaker_domainId,
    deleteSpeaker_speakerId,

    -- * Destructuring the Response
    DeleteSpeakerResponse (..),
    newDeleteSpeakerResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VoiceId.Types

-- | /See:/ 'newDeleteSpeaker' smart constructor.
data DeleteSpeaker = DeleteSpeaker'
  { -- | The identifier of the domain containing the speaker.
    domainId :: Prelude.Text,
    -- | The identifier of the speaker you want to delete.
    speakerId :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSpeaker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainId', 'deleteSpeaker_domainId' - The identifier of the domain containing the speaker.
--
-- 'speakerId', 'deleteSpeaker_speakerId' - The identifier of the speaker you want to delete.
newDeleteSpeaker ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'speakerId'
  Prelude.Text ->
  DeleteSpeaker
newDeleteSpeaker pDomainId_ pSpeakerId_ =
  DeleteSpeaker'
    { domainId = pDomainId_,
      speakerId = Data._Sensitive Lens.# pSpeakerId_
    }

-- | The identifier of the domain containing the speaker.
deleteSpeaker_domainId :: Lens.Lens' DeleteSpeaker Prelude.Text
deleteSpeaker_domainId = Lens.lens (\DeleteSpeaker' {domainId} -> domainId) (\s@DeleteSpeaker' {} a -> s {domainId = a} :: DeleteSpeaker)

-- | The identifier of the speaker you want to delete.
deleteSpeaker_speakerId :: Lens.Lens' DeleteSpeaker Prelude.Text
deleteSpeaker_speakerId = Lens.lens (\DeleteSpeaker' {speakerId} -> speakerId) (\s@DeleteSpeaker' {} a -> s {speakerId = a} :: DeleteSpeaker) Prelude.. Data._Sensitive

instance Core.AWSRequest DeleteSpeaker where
  type
    AWSResponse DeleteSpeaker =
      DeleteSpeakerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteSpeakerResponse'

instance Prelude.Hashable DeleteSpeaker where
  hashWithSalt _salt DeleteSpeaker' {..} =
    _salt
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` speakerId

instance Prelude.NFData DeleteSpeaker where
  rnf DeleteSpeaker' {..} =
    Prelude.rnf domainId `Prelude.seq`
      Prelude.rnf speakerId

instance Data.ToHeaders DeleteSpeaker where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("VoiceID.DeleteSpeaker" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteSpeaker where
  toJSON DeleteSpeaker' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainId" Data..= domainId),
            Prelude.Just ("SpeakerId" Data..= speakerId)
          ]
      )

instance Data.ToPath DeleteSpeaker where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteSpeaker where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSpeakerResponse' smart constructor.
data DeleteSpeakerResponse = DeleteSpeakerResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSpeakerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteSpeakerResponse ::
  DeleteSpeakerResponse
newDeleteSpeakerResponse = DeleteSpeakerResponse'

instance Prelude.NFData DeleteSpeakerResponse where
  rnf _ = ()
