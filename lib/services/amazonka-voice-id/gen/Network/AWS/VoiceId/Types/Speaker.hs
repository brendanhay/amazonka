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
-- Module      : Network.AWS.VoiceId.Types.Speaker
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.VoiceId.Types.Speaker where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.VoiceId.Types.SpeakerStatus

-- | Contains all the information about a speaker.
--
-- /See:/ 'newSpeaker' smart constructor.
data Speaker = Speaker'
  { -- | The current status of the speaker.
    status :: Prelude.Maybe SpeakerStatus,
    -- | The client-provided identifier for the speaker.
    customerSpeakerId :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A timestamp showing when the speaker is created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The identifier of the domain that contains the speaker.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | A timestamp showing the speaker\'s last update.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | The service-generated identifier for the speaker.
    generatedSpeakerId :: Prelude.Maybe Prelude.Text
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
-- 'status', 'speaker_status' - The current status of the speaker.
--
-- 'customerSpeakerId', 'speaker_customerSpeakerId' - The client-provided identifier for the speaker.
--
-- 'createdAt', 'speaker_createdAt' - A timestamp showing when the speaker is created.
--
-- 'domainId', 'speaker_domainId' - The identifier of the domain that contains the speaker.
--
-- 'updatedAt', 'speaker_updatedAt' - A timestamp showing the speaker\'s last update.
--
-- 'generatedSpeakerId', 'speaker_generatedSpeakerId' - The service-generated identifier for the speaker.
newSpeaker ::
  Speaker
newSpeaker =
  Speaker'
    { status = Prelude.Nothing,
      customerSpeakerId = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      domainId = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      generatedSpeakerId = Prelude.Nothing
    }

-- | The current status of the speaker.
speaker_status :: Lens.Lens' Speaker (Prelude.Maybe SpeakerStatus)
speaker_status = Lens.lens (\Speaker' {status} -> status) (\s@Speaker' {} a -> s {status = a} :: Speaker)

-- | The client-provided identifier for the speaker.
speaker_customerSpeakerId :: Lens.Lens' Speaker (Prelude.Maybe Prelude.Text)
speaker_customerSpeakerId = Lens.lens (\Speaker' {customerSpeakerId} -> customerSpeakerId) (\s@Speaker' {} a -> s {customerSpeakerId = a} :: Speaker) Prelude.. Lens.mapping Core._Sensitive

-- | A timestamp showing when the speaker is created.
speaker_createdAt :: Lens.Lens' Speaker (Prelude.Maybe Prelude.UTCTime)
speaker_createdAt = Lens.lens (\Speaker' {createdAt} -> createdAt) (\s@Speaker' {} a -> s {createdAt = a} :: Speaker) Prelude.. Lens.mapping Core._Time

-- | The identifier of the domain that contains the speaker.
speaker_domainId :: Lens.Lens' Speaker (Prelude.Maybe Prelude.Text)
speaker_domainId = Lens.lens (\Speaker' {domainId} -> domainId) (\s@Speaker' {} a -> s {domainId = a} :: Speaker)

-- | A timestamp showing the speaker\'s last update.
speaker_updatedAt :: Lens.Lens' Speaker (Prelude.Maybe Prelude.UTCTime)
speaker_updatedAt = Lens.lens (\Speaker' {updatedAt} -> updatedAt) (\s@Speaker' {} a -> s {updatedAt = a} :: Speaker) Prelude.. Lens.mapping Core._Time

-- | The service-generated identifier for the speaker.
speaker_generatedSpeakerId :: Lens.Lens' Speaker (Prelude.Maybe Prelude.Text)
speaker_generatedSpeakerId = Lens.lens (\Speaker' {generatedSpeakerId} -> generatedSpeakerId) (\s@Speaker' {} a -> s {generatedSpeakerId = a} :: Speaker)

instance Core.FromJSON Speaker where
  parseJSON =
    Core.withObject
      "Speaker"
      ( \x ->
          Speaker'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "CustomerSpeakerId")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "DomainId")
            Prelude.<*> (x Core..:? "UpdatedAt")
            Prelude.<*> (x Core..:? "GeneratedSpeakerId")
      )

instance Prelude.Hashable Speaker

instance Prelude.NFData Speaker
