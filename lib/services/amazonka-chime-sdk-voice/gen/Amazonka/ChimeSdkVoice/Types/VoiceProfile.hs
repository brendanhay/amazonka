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
-- Module      : Amazonka.ChimeSdkVoice.Types.VoiceProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.VoiceProfile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The combination of a voice print and caller ID.
--
-- /See:/ 'newVoiceProfile' smart constructor.
data VoiceProfile = VoiceProfile'
  { -- | The time at which the voice profile was created and enrolled.
    createdTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The time at which a voice profile expires unless you re-enroll the
    -- caller via the @UpdateVoiceProfile@ API.
    expirationTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The time at which the voice profile was last updated.
    updatedTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The ARN of the voice profile.
    voiceProfileArn :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the domain that contains the voice profile.
    voiceProfileDomainId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the voice profile.
    voiceProfileId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VoiceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'voiceProfile_createdTimestamp' - The time at which the voice profile was created and enrolled.
--
-- 'expirationTimestamp', 'voiceProfile_expirationTimestamp' - The time at which a voice profile expires unless you re-enroll the
-- caller via the @UpdateVoiceProfile@ API.
--
-- 'updatedTimestamp', 'voiceProfile_updatedTimestamp' - The time at which the voice profile was last updated.
--
-- 'voiceProfileArn', 'voiceProfile_voiceProfileArn' - The ARN of the voice profile.
--
-- 'voiceProfileDomainId', 'voiceProfile_voiceProfileDomainId' - The ID of the domain that contains the voice profile.
--
-- 'voiceProfileId', 'voiceProfile_voiceProfileId' - The ID of the voice profile.
newVoiceProfile ::
  VoiceProfile
newVoiceProfile =
  VoiceProfile'
    { createdTimestamp = Prelude.Nothing,
      expirationTimestamp = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing,
      voiceProfileArn = Prelude.Nothing,
      voiceProfileDomainId = Prelude.Nothing,
      voiceProfileId = Prelude.Nothing
    }

-- | The time at which the voice profile was created and enrolled.
voiceProfile_createdTimestamp :: Lens.Lens' VoiceProfile (Prelude.Maybe Prelude.UTCTime)
voiceProfile_createdTimestamp = Lens.lens (\VoiceProfile' {createdTimestamp} -> createdTimestamp) (\s@VoiceProfile' {} a -> s {createdTimestamp = a} :: VoiceProfile) Prelude.. Lens.mapping Data._Time

-- | The time at which a voice profile expires unless you re-enroll the
-- caller via the @UpdateVoiceProfile@ API.
voiceProfile_expirationTimestamp :: Lens.Lens' VoiceProfile (Prelude.Maybe Prelude.UTCTime)
voiceProfile_expirationTimestamp = Lens.lens (\VoiceProfile' {expirationTimestamp} -> expirationTimestamp) (\s@VoiceProfile' {} a -> s {expirationTimestamp = a} :: VoiceProfile) Prelude.. Lens.mapping Data._Time

-- | The time at which the voice profile was last updated.
voiceProfile_updatedTimestamp :: Lens.Lens' VoiceProfile (Prelude.Maybe Prelude.UTCTime)
voiceProfile_updatedTimestamp = Lens.lens (\VoiceProfile' {updatedTimestamp} -> updatedTimestamp) (\s@VoiceProfile' {} a -> s {updatedTimestamp = a} :: VoiceProfile) Prelude.. Lens.mapping Data._Time

-- | The ARN of the voice profile.
voiceProfile_voiceProfileArn :: Lens.Lens' VoiceProfile (Prelude.Maybe Prelude.Text)
voiceProfile_voiceProfileArn = Lens.lens (\VoiceProfile' {voiceProfileArn} -> voiceProfileArn) (\s@VoiceProfile' {} a -> s {voiceProfileArn = a} :: VoiceProfile) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the domain that contains the voice profile.
voiceProfile_voiceProfileDomainId :: Lens.Lens' VoiceProfile (Prelude.Maybe Prelude.Text)
voiceProfile_voiceProfileDomainId = Lens.lens (\VoiceProfile' {voiceProfileDomainId} -> voiceProfileDomainId) (\s@VoiceProfile' {} a -> s {voiceProfileDomainId = a} :: VoiceProfile)

-- | The ID of the voice profile.
voiceProfile_voiceProfileId :: Lens.Lens' VoiceProfile (Prelude.Maybe Prelude.Text)
voiceProfile_voiceProfileId = Lens.lens (\VoiceProfile' {voiceProfileId} -> voiceProfileId) (\s@VoiceProfile' {} a -> s {voiceProfileId = a} :: VoiceProfile)

instance Data.FromJSON VoiceProfile where
  parseJSON =
    Data.withObject
      "VoiceProfile"
      ( \x ->
          VoiceProfile'
            Prelude.<$> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "ExpirationTimestamp")
            Prelude.<*> (x Data..:? "UpdatedTimestamp")
            Prelude.<*> (x Data..:? "VoiceProfileArn")
            Prelude.<*> (x Data..:? "VoiceProfileDomainId")
            Prelude.<*> (x Data..:? "VoiceProfileId")
      )

instance Prelude.Hashable VoiceProfile where
  hashWithSalt _salt VoiceProfile' {..} =
    _salt
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` expirationTimestamp
      `Prelude.hashWithSalt` updatedTimestamp
      `Prelude.hashWithSalt` voiceProfileArn
      `Prelude.hashWithSalt` voiceProfileDomainId
      `Prelude.hashWithSalt` voiceProfileId

instance Prelude.NFData VoiceProfile where
  rnf VoiceProfile' {..} =
    Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf expirationTimestamp
      `Prelude.seq` Prelude.rnf updatedTimestamp
      `Prelude.seq` Prelude.rnf voiceProfileArn
      `Prelude.seq` Prelude.rnf voiceProfileDomainId
      `Prelude.seq` Prelude.rnf voiceProfileId
