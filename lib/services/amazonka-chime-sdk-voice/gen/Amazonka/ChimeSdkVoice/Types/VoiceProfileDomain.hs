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
-- Module      : Amazonka.ChimeSdkVoice.Types.VoiceProfileDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.VoiceProfileDomain where

import Amazonka.ChimeSdkVoice.Types.ServerSideEncryptionConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A collection of voice profiles.
--
-- /See:/ 'newVoiceProfileDomain' smart constructor.
data VoiceProfileDomain = VoiceProfileDomain'
  { -- | The time at which the voice profile domain was created.
    createdTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The description of the voice profile domain.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the voice profile domain.
    name :: Prelude.Maybe Prelude.Text,
    -- | A structure that contains the configuration settings for server-side
    -- encryption.
    serverSideEncryptionConfiguration :: Prelude.Maybe ServerSideEncryptionConfiguration,
    -- | The time at which the voice profile was last updated.
    updatedTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The voice profile domain\'s Amazon Resource Number (ARN).
    voiceProfileDomainArn :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the voice profile domain.
    voiceProfileDomainId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VoiceProfileDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'voiceProfileDomain_createdTimestamp' - The time at which the voice profile domain was created.
--
-- 'description', 'voiceProfileDomain_description' - The description of the voice profile domain.
--
-- 'name', 'voiceProfileDomain_name' - The name of the voice profile domain.
--
-- 'serverSideEncryptionConfiguration', 'voiceProfileDomain_serverSideEncryptionConfiguration' - A structure that contains the configuration settings for server-side
-- encryption.
--
-- 'updatedTimestamp', 'voiceProfileDomain_updatedTimestamp' - The time at which the voice profile was last updated.
--
-- 'voiceProfileDomainArn', 'voiceProfileDomain_voiceProfileDomainArn' - The voice profile domain\'s Amazon Resource Number (ARN).
--
-- 'voiceProfileDomainId', 'voiceProfileDomain_voiceProfileDomainId' - The ID of the voice profile domain.
newVoiceProfileDomain ::
  VoiceProfileDomain
newVoiceProfileDomain =
  VoiceProfileDomain'
    { createdTimestamp =
        Prelude.Nothing,
      description = Prelude.Nothing,
      name = Prelude.Nothing,
      serverSideEncryptionConfiguration = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing,
      voiceProfileDomainArn = Prelude.Nothing,
      voiceProfileDomainId = Prelude.Nothing
    }

-- | The time at which the voice profile domain was created.
voiceProfileDomain_createdTimestamp :: Lens.Lens' VoiceProfileDomain (Prelude.Maybe Prelude.UTCTime)
voiceProfileDomain_createdTimestamp = Lens.lens (\VoiceProfileDomain' {createdTimestamp} -> createdTimestamp) (\s@VoiceProfileDomain' {} a -> s {createdTimestamp = a} :: VoiceProfileDomain) Prelude.. Lens.mapping Data._Time

-- | The description of the voice profile domain.
voiceProfileDomain_description :: Lens.Lens' VoiceProfileDomain (Prelude.Maybe Prelude.Text)
voiceProfileDomain_description = Lens.lens (\VoiceProfileDomain' {description} -> description) (\s@VoiceProfileDomain' {} a -> s {description = a} :: VoiceProfileDomain)

-- | The name of the voice profile domain.
voiceProfileDomain_name :: Lens.Lens' VoiceProfileDomain (Prelude.Maybe Prelude.Text)
voiceProfileDomain_name = Lens.lens (\VoiceProfileDomain' {name} -> name) (\s@VoiceProfileDomain' {} a -> s {name = a} :: VoiceProfileDomain)

-- | A structure that contains the configuration settings for server-side
-- encryption.
voiceProfileDomain_serverSideEncryptionConfiguration :: Lens.Lens' VoiceProfileDomain (Prelude.Maybe ServerSideEncryptionConfiguration)
voiceProfileDomain_serverSideEncryptionConfiguration = Lens.lens (\VoiceProfileDomain' {serverSideEncryptionConfiguration} -> serverSideEncryptionConfiguration) (\s@VoiceProfileDomain' {} a -> s {serverSideEncryptionConfiguration = a} :: VoiceProfileDomain)

-- | The time at which the voice profile was last updated.
voiceProfileDomain_updatedTimestamp :: Lens.Lens' VoiceProfileDomain (Prelude.Maybe Prelude.UTCTime)
voiceProfileDomain_updatedTimestamp = Lens.lens (\VoiceProfileDomain' {updatedTimestamp} -> updatedTimestamp) (\s@VoiceProfileDomain' {} a -> s {updatedTimestamp = a} :: VoiceProfileDomain) Prelude.. Lens.mapping Data._Time

-- | The voice profile domain\'s Amazon Resource Number (ARN).
voiceProfileDomain_voiceProfileDomainArn :: Lens.Lens' VoiceProfileDomain (Prelude.Maybe Prelude.Text)
voiceProfileDomain_voiceProfileDomainArn = Lens.lens (\VoiceProfileDomain' {voiceProfileDomainArn} -> voiceProfileDomainArn) (\s@VoiceProfileDomain' {} a -> s {voiceProfileDomainArn = a} :: VoiceProfileDomain) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the voice profile domain.
voiceProfileDomain_voiceProfileDomainId :: Lens.Lens' VoiceProfileDomain (Prelude.Maybe Prelude.Text)
voiceProfileDomain_voiceProfileDomainId = Lens.lens (\VoiceProfileDomain' {voiceProfileDomainId} -> voiceProfileDomainId) (\s@VoiceProfileDomain' {} a -> s {voiceProfileDomainId = a} :: VoiceProfileDomain)

instance Data.FromJSON VoiceProfileDomain where
  parseJSON =
    Data.withObject
      "VoiceProfileDomain"
      ( \x ->
          VoiceProfileDomain'
            Prelude.<$> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ServerSideEncryptionConfiguration")
            Prelude.<*> (x Data..:? "UpdatedTimestamp")
            Prelude.<*> (x Data..:? "VoiceProfileDomainArn")
            Prelude.<*> (x Data..:? "VoiceProfileDomainId")
      )

instance Prelude.Hashable VoiceProfileDomain where
  hashWithSalt _salt VoiceProfileDomain' {..} =
    _salt
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` serverSideEncryptionConfiguration
      `Prelude.hashWithSalt` updatedTimestamp
      `Prelude.hashWithSalt` voiceProfileDomainArn
      `Prelude.hashWithSalt` voiceProfileDomainId

instance Prelude.NFData VoiceProfileDomain where
  rnf VoiceProfileDomain' {..} =
    Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf serverSideEncryptionConfiguration
      `Prelude.seq` Prelude.rnf updatedTimestamp
      `Prelude.seq` Prelude.rnf voiceProfileDomainArn
      `Prelude.seq` Prelude.rnf voiceProfileDomainId
