{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.VoiceId.Types.Speaker
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.Speaker where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VoiceId.Types.SpeakerStatus

-- | Contains all the information about a speaker.
--
-- /See:/ 'newSpeaker' smart constructor.
data Speaker = Speaker'
  { -- | A timestamp showing when the speaker is created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The client-provided identifier for the speaker.
    customerSpeakerId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The identifier of the domain that contains the speaker.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | The service-generated identifier for the speaker.
    generatedSpeakerId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the speaker was last accessed for enrollment,
    -- re-enrollment or a successful authentication. This timestamp is accurate
    -- to one hour.
    lastAccessedAt :: Prelude.Maybe Data.POSIX,
    -- | The current status of the speaker.
    status :: Prelude.Maybe SpeakerStatus,
    -- | A timestamp showing the speaker\'s last update.
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Speaker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'speaker_createdAt' - A timestamp showing when the speaker is created.
--
-- 'customerSpeakerId', 'speaker_customerSpeakerId' - The client-provided identifier for the speaker.
--
-- 'domainId', 'speaker_domainId' - The identifier of the domain that contains the speaker.
--
-- 'generatedSpeakerId', 'speaker_generatedSpeakerId' - The service-generated identifier for the speaker.
--
-- 'lastAccessedAt', 'speaker_lastAccessedAt' - The timestamp when the speaker was last accessed for enrollment,
-- re-enrollment or a successful authentication. This timestamp is accurate
-- to one hour.
--
-- 'status', 'speaker_status' - The current status of the speaker.
--
-- 'updatedAt', 'speaker_updatedAt' - A timestamp showing the speaker\'s last update.
newSpeaker ::
  Speaker
newSpeaker =
  Speaker'
    { createdAt = Prelude.Nothing,
      customerSpeakerId = Prelude.Nothing,
      domainId = Prelude.Nothing,
      generatedSpeakerId = Prelude.Nothing,
      lastAccessedAt = Prelude.Nothing,
      status = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | A timestamp showing when the speaker is created.
speaker_createdAt :: Lens.Lens' Speaker (Prelude.Maybe Prelude.UTCTime)
speaker_createdAt = Lens.lens (\Speaker' {createdAt} -> createdAt) (\s@Speaker' {} a -> s {createdAt = a} :: Speaker) Prelude.. Lens.mapping Data._Time

-- | The client-provided identifier for the speaker.
speaker_customerSpeakerId :: Lens.Lens' Speaker (Prelude.Maybe Prelude.Text)
speaker_customerSpeakerId = Lens.lens (\Speaker' {customerSpeakerId} -> customerSpeakerId) (\s@Speaker' {} a -> s {customerSpeakerId = a} :: Speaker) Prelude.. Lens.mapping Data._Sensitive

-- | The identifier of the domain that contains the speaker.
speaker_domainId :: Lens.Lens' Speaker (Prelude.Maybe Prelude.Text)
speaker_domainId = Lens.lens (\Speaker' {domainId} -> domainId) (\s@Speaker' {} a -> s {domainId = a} :: Speaker)

-- | The service-generated identifier for the speaker.
speaker_generatedSpeakerId :: Lens.Lens' Speaker (Prelude.Maybe Prelude.Text)
speaker_generatedSpeakerId = Lens.lens (\Speaker' {generatedSpeakerId} -> generatedSpeakerId) (\s@Speaker' {} a -> s {generatedSpeakerId = a} :: Speaker)

-- | The timestamp when the speaker was last accessed for enrollment,
-- re-enrollment or a successful authentication. This timestamp is accurate
-- to one hour.
speaker_lastAccessedAt :: Lens.Lens' Speaker (Prelude.Maybe Prelude.UTCTime)
speaker_lastAccessedAt = Lens.lens (\Speaker' {lastAccessedAt} -> lastAccessedAt) (\s@Speaker' {} a -> s {lastAccessedAt = a} :: Speaker) Prelude.. Lens.mapping Data._Time

-- | The current status of the speaker.
speaker_status :: Lens.Lens' Speaker (Prelude.Maybe SpeakerStatus)
speaker_status = Lens.lens (\Speaker' {status} -> status) (\s@Speaker' {} a -> s {status = a} :: Speaker)

-- | A timestamp showing the speaker\'s last update.
speaker_updatedAt :: Lens.Lens' Speaker (Prelude.Maybe Prelude.UTCTime)
speaker_updatedAt = Lens.lens (\Speaker' {updatedAt} -> updatedAt) (\s@Speaker' {} a -> s {updatedAt = a} :: Speaker) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Speaker where
  parseJSON =
    Data.withObject
      "Speaker"
      ( \x ->
          Speaker'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "CustomerSpeakerId")
            Prelude.<*> (x Data..:? "DomainId")
            Prelude.<*> (x Data..:? "GeneratedSpeakerId")
            Prelude.<*> (x Data..:? "LastAccessedAt")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "UpdatedAt")
      )

instance Prelude.Hashable Speaker where
  hashWithSalt _salt Speaker' {..} =
    _salt `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` customerSpeakerId
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` generatedSpeakerId
      `Prelude.hashWithSalt` lastAccessedAt
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData Speaker where
  rnf Speaker' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf customerSpeakerId
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf generatedSpeakerId
      `Prelude.seq` Prelude.rnf lastAccessedAt
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updatedAt
