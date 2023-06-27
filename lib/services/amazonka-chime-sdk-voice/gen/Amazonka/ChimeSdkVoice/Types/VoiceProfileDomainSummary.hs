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
-- Module      : Amazonka.ChimeSdkVoice.Types.VoiceProfileDomainSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.VoiceProfileDomainSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A high-level overview of a voice profile domain.
--
-- /See:/ 'newVoiceProfileDomainSummary' smart constructor.
data VoiceProfileDomainSummary = VoiceProfileDomainSummary'
  { -- | The time at which the voice profile domain summary was created.
    createdTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | Describes the voice profile domain summary.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the voice profile domain summary.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time at which the voice profile domain summary was last updated.
    updatedTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The ARN of a voice profile in a voice profile domain summary.
    voiceProfileDomainArn :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the voice profile domain summary.
    voiceProfileDomainId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VoiceProfileDomainSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'voiceProfileDomainSummary_createdTimestamp' - The time at which the voice profile domain summary was created.
--
-- 'description', 'voiceProfileDomainSummary_description' - Describes the voice profile domain summary.
--
-- 'name', 'voiceProfileDomainSummary_name' - The name of the voice profile domain summary.
--
-- 'updatedTimestamp', 'voiceProfileDomainSummary_updatedTimestamp' - The time at which the voice profile domain summary was last updated.
--
-- 'voiceProfileDomainArn', 'voiceProfileDomainSummary_voiceProfileDomainArn' - The ARN of a voice profile in a voice profile domain summary.
--
-- 'voiceProfileDomainId', 'voiceProfileDomainSummary_voiceProfileDomainId' - The ID of the voice profile domain summary.
newVoiceProfileDomainSummary ::
  VoiceProfileDomainSummary
newVoiceProfileDomainSummary =
  VoiceProfileDomainSummary'
    { createdTimestamp =
        Prelude.Nothing,
      description = Prelude.Nothing,
      name = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing,
      voiceProfileDomainArn = Prelude.Nothing,
      voiceProfileDomainId = Prelude.Nothing
    }

-- | The time at which the voice profile domain summary was created.
voiceProfileDomainSummary_createdTimestamp :: Lens.Lens' VoiceProfileDomainSummary (Prelude.Maybe Prelude.UTCTime)
voiceProfileDomainSummary_createdTimestamp = Lens.lens (\VoiceProfileDomainSummary' {createdTimestamp} -> createdTimestamp) (\s@VoiceProfileDomainSummary' {} a -> s {createdTimestamp = a} :: VoiceProfileDomainSummary) Prelude.. Lens.mapping Data._Time

-- | Describes the voice profile domain summary.
voiceProfileDomainSummary_description :: Lens.Lens' VoiceProfileDomainSummary (Prelude.Maybe Prelude.Text)
voiceProfileDomainSummary_description = Lens.lens (\VoiceProfileDomainSummary' {description} -> description) (\s@VoiceProfileDomainSummary' {} a -> s {description = a} :: VoiceProfileDomainSummary)

-- | The name of the voice profile domain summary.
voiceProfileDomainSummary_name :: Lens.Lens' VoiceProfileDomainSummary (Prelude.Maybe Prelude.Text)
voiceProfileDomainSummary_name = Lens.lens (\VoiceProfileDomainSummary' {name} -> name) (\s@VoiceProfileDomainSummary' {} a -> s {name = a} :: VoiceProfileDomainSummary)

-- | The time at which the voice profile domain summary was last updated.
voiceProfileDomainSummary_updatedTimestamp :: Lens.Lens' VoiceProfileDomainSummary (Prelude.Maybe Prelude.UTCTime)
voiceProfileDomainSummary_updatedTimestamp = Lens.lens (\VoiceProfileDomainSummary' {updatedTimestamp} -> updatedTimestamp) (\s@VoiceProfileDomainSummary' {} a -> s {updatedTimestamp = a} :: VoiceProfileDomainSummary) Prelude.. Lens.mapping Data._Time

-- | The ARN of a voice profile in a voice profile domain summary.
voiceProfileDomainSummary_voiceProfileDomainArn :: Lens.Lens' VoiceProfileDomainSummary (Prelude.Maybe Prelude.Text)
voiceProfileDomainSummary_voiceProfileDomainArn = Lens.lens (\VoiceProfileDomainSummary' {voiceProfileDomainArn} -> voiceProfileDomainArn) (\s@VoiceProfileDomainSummary' {} a -> s {voiceProfileDomainArn = a} :: VoiceProfileDomainSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the voice profile domain summary.
voiceProfileDomainSummary_voiceProfileDomainId :: Lens.Lens' VoiceProfileDomainSummary (Prelude.Maybe Prelude.Text)
voiceProfileDomainSummary_voiceProfileDomainId = Lens.lens (\VoiceProfileDomainSummary' {voiceProfileDomainId} -> voiceProfileDomainId) (\s@VoiceProfileDomainSummary' {} a -> s {voiceProfileDomainId = a} :: VoiceProfileDomainSummary)

instance Data.FromJSON VoiceProfileDomainSummary where
  parseJSON =
    Data.withObject
      "VoiceProfileDomainSummary"
      ( \x ->
          VoiceProfileDomainSummary'
            Prelude.<$> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "UpdatedTimestamp")
            Prelude.<*> (x Data..:? "VoiceProfileDomainArn")
            Prelude.<*> (x Data..:? "VoiceProfileDomainId")
      )

instance Prelude.Hashable VoiceProfileDomainSummary where
  hashWithSalt _salt VoiceProfileDomainSummary' {..} =
    _salt
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` updatedTimestamp
      `Prelude.hashWithSalt` voiceProfileDomainArn
      `Prelude.hashWithSalt` voiceProfileDomainId

instance Prelude.NFData VoiceProfileDomainSummary where
  rnf VoiceProfileDomainSummary' {..} =
    Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf updatedTimestamp
      `Prelude.seq` Prelude.rnf voiceProfileDomainArn
      `Prelude.seq` Prelude.rnf voiceProfileDomainId
