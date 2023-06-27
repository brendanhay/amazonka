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
-- Module      : Amazonka.ChimeSdkVoice.Types.VoiceProfileSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.VoiceProfileSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A high-level summary of a voice profile.
--
-- /See:/ 'newVoiceProfileSummary' smart constructor.
data VoiceProfileSummary = VoiceProfileSummary'
  { -- | The time at which a voice profile summary was created.
    createdTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | Extends the life of the voice profile. You can use @UpdateVoiceProfile@
    -- to refresh an existing voice profile\'s voice print and extend the life
    -- of the summary.
    expirationTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The time at which a voice profile summary was last updated.
    updatedTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The ARN of the voice profile in a voice profile summary.
    voiceProfileArn :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the voice profile domain in a voice profile summary.
    voiceProfileDomainId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the voice profile in a voice profile summary.
    voiceProfileId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VoiceProfileSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'voiceProfileSummary_createdTimestamp' - The time at which a voice profile summary was created.
--
-- 'expirationTimestamp', 'voiceProfileSummary_expirationTimestamp' - Extends the life of the voice profile. You can use @UpdateVoiceProfile@
-- to refresh an existing voice profile\'s voice print and extend the life
-- of the summary.
--
-- 'updatedTimestamp', 'voiceProfileSummary_updatedTimestamp' - The time at which a voice profile summary was last updated.
--
-- 'voiceProfileArn', 'voiceProfileSummary_voiceProfileArn' - The ARN of the voice profile in a voice profile summary.
--
-- 'voiceProfileDomainId', 'voiceProfileSummary_voiceProfileDomainId' - The ID of the voice profile domain in a voice profile summary.
--
-- 'voiceProfileId', 'voiceProfileSummary_voiceProfileId' - The ID of the voice profile in a voice profile summary.
newVoiceProfileSummary ::
  VoiceProfileSummary
newVoiceProfileSummary =
  VoiceProfileSummary'
    { createdTimestamp =
        Prelude.Nothing,
      expirationTimestamp = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing,
      voiceProfileArn = Prelude.Nothing,
      voiceProfileDomainId = Prelude.Nothing,
      voiceProfileId = Prelude.Nothing
    }

-- | The time at which a voice profile summary was created.
voiceProfileSummary_createdTimestamp :: Lens.Lens' VoiceProfileSummary (Prelude.Maybe Prelude.UTCTime)
voiceProfileSummary_createdTimestamp = Lens.lens (\VoiceProfileSummary' {createdTimestamp} -> createdTimestamp) (\s@VoiceProfileSummary' {} a -> s {createdTimestamp = a} :: VoiceProfileSummary) Prelude.. Lens.mapping Data._Time

-- | Extends the life of the voice profile. You can use @UpdateVoiceProfile@
-- to refresh an existing voice profile\'s voice print and extend the life
-- of the summary.
voiceProfileSummary_expirationTimestamp :: Lens.Lens' VoiceProfileSummary (Prelude.Maybe Prelude.UTCTime)
voiceProfileSummary_expirationTimestamp = Lens.lens (\VoiceProfileSummary' {expirationTimestamp} -> expirationTimestamp) (\s@VoiceProfileSummary' {} a -> s {expirationTimestamp = a} :: VoiceProfileSummary) Prelude.. Lens.mapping Data._Time

-- | The time at which a voice profile summary was last updated.
voiceProfileSummary_updatedTimestamp :: Lens.Lens' VoiceProfileSummary (Prelude.Maybe Prelude.UTCTime)
voiceProfileSummary_updatedTimestamp = Lens.lens (\VoiceProfileSummary' {updatedTimestamp} -> updatedTimestamp) (\s@VoiceProfileSummary' {} a -> s {updatedTimestamp = a} :: VoiceProfileSummary) Prelude.. Lens.mapping Data._Time

-- | The ARN of the voice profile in a voice profile summary.
voiceProfileSummary_voiceProfileArn :: Lens.Lens' VoiceProfileSummary (Prelude.Maybe Prelude.Text)
voiceProfileSummary_voiceProfileArn = Lens.lens (\VoiceProfileSummary' {voiceProfileArn} -> voiceProfileArn) (\s@VoiceProfileSummary' {} a -> s {voiceProfileArn = a} :: VoiceProfileSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the voice profile domain in a voice profile summary.
voiceProfileSummary_voiceProfileDomainId :: Lens.Lens' VoiceProfileSummary (Prelude.Maybe Prelude.Text)
voiceProfileSummary_voiceProfileDomainId = Lens.lens (\VoiceProfileSummary' {voiceProfileDomainId} -> voiceProfileDomainId) (\s@VoiceProfileSummary' {} a -> s {voiceProfileDomainId = a} :: VoiceProfileSummary)

-- | The ID of the voice profile in a voice profile summary.
voiceProfileSummary_voiceProfileId :: Lens.Lens' VoiceProfileSummary (Prelude.Maybe Prelude.Text)
voiceProfileSummary_voiceProfileId = Lens.lens (\VoiceProfileSummary' {voiceProfileId} -> voiceProfileId) (\s@VoiceProfileSummary' {} a -> s {voiceProfileId = a} :: VoiceProfileSummary)

instance Data.FromJSON VoiceProfileSummary where
  parseJSON =
    Data.withObject
      "VoiceProfileSummary"
      ( \x ->
          VoiceProfileSummary'
            Prelude.<$> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "ExpirationTimestamp")
            Prelude.<*> (x Data..:? "UpdatedTimestamp")
            Prelude.<*> (x Data..:? "VoiceProfileArn")
            Prelude.<*> (x Data..:? "VoiceProfileDomainId")
            Prelude.<*> (x Data..:? "VoiceProfileId")
      )

instance Prelude.Hashable VoiceProfileSummary where
  hashWithSalt _salt VoiceProfileSummary' {..} =
    _salt
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` expirationTimestamp
      `Prelude.hashWithSalt` updatedTimestamp
      `Prelude.hashWithSalt` voiceProfileArn
      `Prelude.hashWithSalt` voiceProfileDomainId
      `Prelude.hashWithSalt` voiceProfileId

instance Prelude.NFData VoiceProfileSummary where
  rnf VoiceProfileSummary' {..} =
    Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf expirationTimestamp
      `Prelude.seq` Prelude.rnf updatedTimestamp
      `Prelude.seq` Prelude.rnf voiceProfileArn
      `Prelude.seq` Prelude.rnf voiceProfileDomainId
      `Prelude.seq` Prelude.rnf voiceProfileId
