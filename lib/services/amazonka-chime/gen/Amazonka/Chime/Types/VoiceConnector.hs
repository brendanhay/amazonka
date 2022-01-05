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
-- Module      : Amazonka.Chime.Types.VoiceConnector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.VoiceConnector where

import Amazonka.Chime.Types.VoiceConnectorAwsRegion
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Chime Voice Connector configuration, including outbound host
-- name and encryption settings.
--
-- /See:/ 'newVoiceConnector' smart constructor.
data VoiceConnector = VoiceConnector'
  { -- | The updated Amazon Chime Voice Connector timestamp, in ISO 8601 format.
    updatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The outbound host name for the Amazon Chime Voice Connector.
    outboundHostName :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon Chime Voice Connector.
    name :: Prelude.Maybe Prelude.Text,
    -- | Designates whether encryption is required for the Amazon Chime Voice
    -- Connector.
    requireEncryption :: Prelude.Maybe Prelude.Bool,
    -- | The AWS Region in which the Amazon Chime Voice Connector is created.
    -- Default: @us-east-1@.
    awsRegion :: Prelude.Maybe VoiceConnectorAwsRegion,
    -- | The Amazon Chime Voice Connector ID.
    voiceConnectorId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the specified Amazon Chime Voice Connector.
    voiceConnectorArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Chime Voice Connector creation timestamp, in ISO 8601 format.
    createdTimestamp :: Prelude.Maybe Core.POSIX
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
-- 'updatedTimestamp', 'voiceConnector_updatedTimestamp' - The updated Amazon Chime Voice Connector timestamp, in ISO 8601 format.
--
-- 'outboundHostName', 'voiceConnector_outboundHostName' - The outbound host name for the Amazon Chime Voice Connector.
--
-- 'name', 'voiceConnector_name' - The name of the Amazon Chime Voice Connector.
--
-- 'requireEncryption', 'voiceConnector_requireEncryption' - Designates whether encryption is required for the Amazon Chime Voice
-- Connector.
--
-- 'awsRegion', 'voiceConnector_awsRegion' - The AWS Region in which the Amazon Chime Voice Connector is created.
-- Default: @us-east-1@.
--
-- 'voiceConnectorId', 'voiceConnector_voiceConnectorId' - The Amazon Chime Voice Connector ID.
--
-- 'voiceConnectorArn', 'voiceConnector_voiceConnectorArn' - The ARN of the specified Amazon Chime Voice Connector.
--
-- 'createdTimestamp', 'voiceConnector_createdTimestamp' - The Amazon Chime Voice Connector creation timestamp, in ISO 8601 format.
newVoiceConnector ::
  VoiceConnector
newVoiceConnector =
  VoiceConnector'
    { updatedTimestamp = Prelude.Nothing,
      outboundHostName = Prelude.Nothing,
      name = Prelude.Nothing,
      requireEncryption = Prelude.Nothing,
      awsRegion = Prelude.Nothing,
      voiceConnectorId = Prelude.Nothing,
      voiceConnectorArn = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing
    }

-- | The updated Amazon Chime Voice Connector timestamp, in ISO 8601 format.
voiceConnector_updatedTimestamp :: Lens.Lens' VoiceConnector (Prelude.Maybe Prelude.UTCTime)
voiceConnector_updatedTimestamp = Lens.lens (\VoiceConnector' {updatedTimestamp} -> updatedTimestamp) (\s@VoiceConnector' {} a -> s {updatedTimestamp = a} :: VoiceConnector) Prelude.. Lens.mapping Core._Time

-- | The outbound host name for the Amazon Chime Voice Connector.
voiceConnector_outboundHostName :: Lens.Lens' VoiceConnector (Prelude.Maybe Prelude.Text)
voiceConnector_outboundHostName = Lens.lens (\VoiceConnector' {outboundHostName} -> outboundHostName) (\s@VoiceConnector' {} a -> s {outboundHostName = a} :: VoiceConnector)

-- | The name of the Amazon Chime Voice Connector.
voiceConnector_name :: Lens.Lens' VoiceConnector (Prelude.Maybe Prelude.Text)
voiceConnector_name = Lens.lens (\VoiceConnector' {name} -> name) (\s@VoiceConnector' {} a -> s {name = a} :: VoiceConnector)

-- | Designates whether encryption is required for the Amazon Chime Voice
-- Connector.
voiceConnector_requireEncryption :: Lens.Lens' VoiceConnector (Prelude.Maybe Prelude.Bool)
voiceConnector_requireEncryption = Lens.lens (\VoiceConnector' {requireEncryption} -> requireEncryption) (\s@VoiceConnector' {} a -> s {requireEncryption = a} :: VoiceConnector)

-- | The AWS Region in which the Amazon Chime Voice Connector is created.
-- Default: @us-east-1@.
voiceConnector_awsRegion :: Lens.Lens' VoiceConnector (Prelude.Maybe VoiceConnectorAwsRegion)
voiceConnector_awsRegion = Lens.lens (\VoiceConnector' {awsRegion} -> awsRegion) (\s@VoiceConnector' {} a -> s {awsRegion = a} :: VoiceConnector)

-- | The Amazon Chime Voice Connector ID.
voiceConnector_voiceConnectorId :: Lens.Lens' VoiceConnector (Prelude.Maybe Prelude.Text)
voiceConnector_voiceConnectorId = Lens.lens (\VoiceConnector' {voiceConnectorId} -> voiceConnectorId) (\s@VoiceConnector' {} a -> s {voiceConnectorId = a} :: VoiceConnector)

-- | The ARN of the specified Amazon Chime Voice Connector.
voiceConnector_voiceConnectorArn :: Lens.Lens' VoiceConnector (Prelude.Maybe Prelude.Text)
voiceConnector_voiceConnectorArn = Lens.lens (\VoiceConnector' {voiceConnectorArn} -> voiceConnectorArn) (\s@VoiceConnector' {} a -> s {voiceConnectorArn = a} :: VoiceConnector)

-- | The Amazon Chime Voice Connector creation timestamp, in ISO 8601 format.
voiceConnector_createdTimestamp :: Lens.Lens' VoiceConnector (Prelude.Maybe Prelude.UTCTime)
voiceConnector_createdTimestamp = Lens.lens (\VoiceConnector' {createdTimestamp} -> createdTimestamp) (\s@VoiceConnector' {} a -> s {createdTimestamp = a} :: VoiceConnector) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON VoiceConnector where
  parseJSON =
    Core.withObject
      "VoiceConnector"
      ( \x ->
          VoiceConnector'
            Prelude.<$> (x Core..:? "UpdatedTimestamp")
            Prelude.<*> (x Core..:? "OutboundHostName")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "RequireEncryption")
            Prelude.<*> (x Core..:? "AwsRegion")
            Prelude.<*> (x Core..:? "VoiceConnectorId")
            Prelude.<*> (x Core..:? "VoiceConnectorArn")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
      )

instance Prelude.Hashable VoiceConnector where
  hashWithSalt _salt VoiceConnector' {..} =
    _salt `Prelude.hashWithSalt` updatedTimestamp
      `Prelude.hashWithSalt` outboundHostName
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` requireEncryption
      `Prelude.hashWithSalt` awsRegion
      `Prelude.hashWithSalt` voiceConnectorId
      `Prelude.hashWithSalt` voiceConnectorArn
      `Prelude.hashWithSalt` createdTimestamp

instance Prelude.NFData VoiceConnector where
  rnf VoiceConnector' {..} =
    Prelude.rnf updatedTimestamp
      `Prelude.seq` Prelude.rnf outboundHostName
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf requireEncryption
      `Prelude.seq` Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf voiceConnectorId
      `Prelude.seq` Prelude.rnf voiceConnectorArn
      `Prelude.seq` Prelude.rnf createdTimestamp
