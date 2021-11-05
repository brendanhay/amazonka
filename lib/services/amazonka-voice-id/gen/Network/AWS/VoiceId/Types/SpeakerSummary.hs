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
-- Module      : Network.AWS.VoiceId.Types.SpeakerSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.VoiceId.Types.SpeakerSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.VoiceId.Types.SpeakerStatus

-- | Contains a summary of information about a speaker.
--
-- /See:/ 'newSpeakerSummary' smart constructor.
data SpeakerSummary = SpeakerSummary'
  { -- | The current status of the speaker.
    status :: Prelude.Maybe SpeakerStatus,
    -- | The client-provided identifier for the speaker.
    customerSpeakerId :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A timestamp showing the speaker\'s creation time.
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
-- Create a value of 'SpeakerSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'speakerSummary_status' - The current status of the speaker.
--
-- 'customerSpeakerId', 'speakerSummary_customerSpeakerId' - The client-provided identifier for the speaker.
--
-- 'createdAt', 'speakerSummary_createdAt' - A timestamp showing the speaker\'s creation time.
--
-- 'domainId', 'speakerSummary_domainId' - The identifier of the domain that contains the speaker.
--
-- 'updatedAt', 'speakerSummary_updatedAt' - A timestamp showing the speaker\'s last update.
--
-- 'generatedSpeakerId', 'speakerSummary_generatedSpeakerId' - The service-generated identifier for the speaker.
newSpeakerSummary ::
  SpeakerSummary
newSpeakerSummary =
  SpeakerSummary'
    { status = Prelude.Nothing,
      customerSpeakerId = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      domainId = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      generatedSpeakerId = Prelude.Nothing
    }

-- | The current status of the speaker.
speakerSummary_status :: Lens.Lens' SpeakerSummary (Prelude.Maybe SpeakerStatus)
speakerSummary_status = Lens.lens (\SpeakerSummary' {status} -> status) (\s@SpeakerSummary' {} a -> s {status = a} :: SpeakerSummary)

-- | The client-provided identifier for the speaker.
speakerSummary_customerSpeakerId :: Lens.Lens' SpeakerSummary (Prelude.Maybe Prelude.Text)
speakerSummary_customerSpeakerId = Lens.lens (\SpeakerSummary' {customerSpeakerId} -> customerSpeakerId) (\s@SpeakerSummary' {} a -> s {customerSpeakerId = a} :: SpeakerSummary) Prelude.. Lens.mapping Core._Sensitive

-- | A timestamp showing the speaker\'s creation time.
speakerSummary_createdAt :: Lens.Lens' SpeakerSummary (Prelude.Maybe Prelude.UTCTime)
speakerSummary_createdAt = Lens.lens (\SpeakerSummary' {createdAt} -> createdAt) (\s@SpeakerSummary' {} a -> s {createdAt = a} :: SpeakerSummary) Prelude.. Lens.mapping Core._Time

-- | The identifier of the domain that contains the speaker.
speakerSummary_domainId :: Lens.Lens' SpeakerSummary (Prelude.Maybe Prelude.Text)
speakerSummary_domainId = Lens.lens (\SpeakerSummary' {domainId} -> domainId) (\s@SpeakerSummary' {} a -> s {domainId = a} :: SpeakerSummary)

-- | A timestamp showing the speaker\'s last update.
speakerSummary_updatedAt :: Lens.Lens' SpeakerSummary (Prelude.Maybe Prelude.UTCTime)
speakerSummary_updatedAt = Lens.lens (\SpeakerSummary' {updatedAt} -> updatedAt) (\s@SpeakerSummary' {} a -> s {updatedAt = a} :: SpeakerSummary) Prelude.. Lens.mapping Core._Time

-- | The service-generated identifier for the speaker.
speakerSummary_generatedSpeakerId :: Lens.Lens' SpeakerSummary (Prelude.Maybe Prelude.Text)
speakerSummary_generatedSpeakerId = Lens.lens (\SpeakerSummary' {generatedSpeakerId} -> generatedSpeakerId) (\s@SpeakerSummary' {} a -> s {generatedSpeakerId = a} :: SpeakerSummary)

instance Core.FromJSON SpeakerSummary where
  parseJSON =
    Core.withObject
      "SpeakerSummary"
      ( \x ->
          SpeakerSummary'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "CustomerSpeakerId")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "DomainId")
            Prelude.<*> (x Core..:? "UpdatedAt")
            Prelude.<*> (x Core..:? "GeneratedSpeakerId")
      )

instance Prelude.Hashable SpeakerSummary

instance Prelude.NFData SpeakerSummary
