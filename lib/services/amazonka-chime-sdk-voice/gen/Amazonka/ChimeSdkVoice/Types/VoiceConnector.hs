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
-- Module      : Amazonka.ChimeSdkVoice.Types.VoiceConnector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.VoiceConnector where

import Amazonka.ChimeSdkVoice.Types.VoiceConnectorAwsRegion
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newVoiceConnector' smart constructor.
data VoiceConnector = VoiceConnector'
  { awsRegion :: Prelude.Maybe VoiceConnectorAwsRegion,
    createdTimestamp :: Prelude.Maybe Data.ISO8601,
    name :: Prelude.Maybe Prelude.Text,
    outboundHostName :: Prelude.Maybe Prelude.Text,
    requireEncryption :: Prelude.Maybe Prelude.Bool,
    updatedTimestamp :: Prelude.Maybe Data.ISO8601,
    voiceConnectorArn :: Prelude.Maybe Prelude.Text,
    voiceConnectorId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VoiceConnector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsRegion', 'voiceConnector_awsRegion' - Undocumented member.
--
-- 'createdTimestamp', 'voiceConnector_createdTimestamp' - Undocumented member.
--
-- 'name', 'voiceConnector_name' - Undocumented member.
--
-- 'outboundHostName', 'voiceConnector_outboundHostName' - Undocumented member.
--
-- 'requireEncryption', 'voiceConnector_requireEncryption' - Undocumented member.
--
-- 'updatedTimestamp', 'voiceConnector_updatedTimestamp' - Undocumented member.
--
-- 'voiceConnectorArn', 'voiceConnector_voiceConnectorArn' - Undocumented member.
--
-- 'voiceConnectorId', 'voiceConnector_voiceConnectorId' - Undocumented member.
newVoiceConnector ::
  VoiceConnector
newVoiceConnector =
  VoiceConnector'
    { awsRegion = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      name = Prelude.Nothing,
      outboundHostName = Prelude.Nothing,
      requireEncryption = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing,
      voiceConnectorArn = Prelude.Nothing,
      voiceConnectorId = Prelude.Nothing
    }

-- | Undocumented member.
voiceConnector_awsRegion :: Lens.Lens' VoiceConnector (Prelude.Maybe VoiceConnectorAwsRegion)
voiceConnector_awsRegion = Lens.lens (\VoiceConnector' {awsRegion} -> awsRegion) (\s@VoiceConnector' {} a -> s {awsRegion = a} :: VoiceConnector)

-- | Undocumented member.
voiceConnector_createdTimestamp :: Lens.Lens' VoiceConnector (Prelude.Maybe Prelude.UTCTime)
voiceConnector_createdTimestamp = Lens.lens (\VoiceConnector' {createdTimestamp} -> createdTimestamp) (\s@VoiceConnector' {} a -> s {createdTimestamp = a} :: VoiceConnector) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
voiceConnector_name :: Lens.Lens' VoiceConnector (Prelude.Maybe Prelude.Text)
voiceConnector_name = Lens.lens (\VoiceConnector' {name} -> name) (\s@VoiceConnector' {} a -> s {name = a} :: VoiceConnector)

-- | Undocumented member.
voiceConnector_outboundHostName :: Lens.Lens' VoiceConnector (Prelude.Maybe Prelude.Text)
voiceConnector_outboundHostName = Lens.lens (\VoiceConnector' {outboundHostName} -> outboundHostName) (\s@VoiceConnector' {} a -> s {outboundHostName = a} :: VoiceConnector)

-- | Undocumented member.
voiceConnector_requireEncryption :: Lens.Lens' VoiceConnector (Prelude.Maybe Prelude.Bool)
voiceConnector_requireEncryption = Lens.lens (\VoiceConnector' {requireEncryption} -> requireEncryption) (\s@VoiceConnector' {} a -> s {requireEncryption = a} :: VoiceConnector)

-- | Undocumented member.
voiceConnector_updatedTimestamp :: Lens.Lens' VoiceConnector (Prelude.Maybe Prelude.UTCTime)
voiceConnector_updatedTimestamp = Lens.lens (\VoiceConnector' {updatedTimestamp} -> updatedTimestamp) (\s@VoiceConnector' {} a -> s {updatedTimestamp = a} :: VoiceConnector) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
voiceConnector_voiceConnectorArn :: Lens.Lens' VoiceConnector (Prelude.Maybe Prelude.Text)
voiceConnector_voiceConnectorArn = Lens.lens (\VoiceConnector' {voiceConnectorArn} -> voiceConnectorArn) (\s@VoiceConnector' {} a -> s {voiceConnectorArn = a} :: VoiceConnector)

-- | Undocumented member.
voiceConnector_voiceConnectorId :: Lens.Lens' VoiceConnector (Prelude.Maybe Prelude.Text)
voiceConnector_voiceConnectorId = Lens.lens (\VoiceConnector' {voiceConnectorId} -> voiceConnectorId) (\s@VoiceConnector' {} a -> s {voiceConnectorId = a} :: VoiceConnector)

instance Data.FromJSON VoiceConnector where
  parseJSON =
    Data.withObject
      "VoiceConnector"
      ( \x ->
          VoiceConnector'
            Prelude.<$> (x Data..:? "AwsRegion")
            Prelude.<*> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OutboundHostName")
            Prelude.<*> (x Data..:? "RequireEncryption")
            Prelude.<*> (x Data..:? "UpdatedTimestamp")
            Prelude.<*> (x Data..:? "VoiceConnectorArn")
            Prelude.<*> (x Data..:? "VoiceConnectorId")
      )

instance Prelude.Hashable VoiceConnector where
  hashWithSalt _salt VoiceConnector' {..} =
    _salt
      `Prelude.hashWithSalt` awsRegion
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` outboundHostName
      `Prelude.hashWithSalt` requireEncryption
      `Prelude.hashWithSalt` updatedTimestamp
      `Prelude.hashWithSalt` voiceConnectorArn
      `Prelude.hashWithSalt` voiceConnectorId

instance Prelude.NFData VoiceConnector where
  rnf VoiceConnector' {..} =
    Prelude.rnf awsRegion `Prelude.seq`
      Prelude.rnf createdTimestamp `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf outboundHostName `Prelude.seq`
            Prelude.rnf requireEncryption `Prelude.seq`
              Prelude.rnf updatedTimestamp `Prelude.seq`
                Prelude.rnf voiceConnectorArn `Prelude.seq`
                  Prelude.rnf voiceConnectorId
