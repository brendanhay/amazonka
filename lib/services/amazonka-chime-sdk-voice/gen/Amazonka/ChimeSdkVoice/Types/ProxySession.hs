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
-- Module      : Amazonka.ChimeSdkVoice.Types.ProxySession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.ProxySession where

import Amazonka.ChimeSdkVoice.Types.Capability
import Amazonka.ChimeSdkVoice.Types.GeoMatchLevel
import Amazonka.ChimeSdkVoice.Types.GeoMatchParams
import Amazonka.ChimeSdkVoice.Types.NumberSelectionBehavior
import Amazonka.ChimeSdkVoice.Types.Participant
import Amazonka.ChimeSdkVoice.Types.ProxySessionStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newProxySession' smart constructor.
data ProxySession = ProxySession'
  { capabilities :: Prelude.Maybe [Capability],
    createdTimestamp :: Prelude.Maybe Data.ISO8601,
    endedTimestamp :: Prelude.Maybe Data.ISO8601,
    expiryMinutes :: Prelude.Maybe Prelude.Natural,
    geoMatchLevel :: Prelude.Maybe GeoMatchLevel,
    geoMatchParams :: Prelude.Maybe GeoMatchParams,
    name :: Prelude.Maybe Prelude.Text,
    numberSelectionBehavior :: Prelude.Maybe NumberSelectionBehavior,
    participants :: Prelude.Maybe [Participant],
    proxySessionId :: Prelude.Maybe Prelude.Text,
    status :: Prelude.Maybe ProxySessionStatus,
    updatedTimestamp :: Prelude.Maybe Data.ISO8601,
    voiceConnectorId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProxySession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capabilities', 'proxySession_capabilities' - Undocumented member.
--
-- 'createdTimestamp', 'proxySession_createdTimestamp' - Undocumented member.
--
-- 'endedTimestamp', 'proxySession_endedTimestamp' - Undocumented member.
--
-- 'expiryMinutes', 'proxySession_expiryMinutes' - Undocumented member.
--
-- 'geoMatchLevel', 'proxySession_geoMatchLevel' - Undocumented member.
--
-- 'geoMatchParams', 'proxySession_geoMatchParams' - Undocumented member.
--
-- 'name', 'proxySession_name' - Undocumented member.
--
-- 'numberSelectionBehavior', 'proxySession_numberSelectionBehavior' - Undocumented member.
--
-- 'participants', 'proxySession_participants' - Undocumented member.
--
-- 'proxySessionId', 'proxySession_proxySessionId' - Undocumented member.
--
-- 'status', 'proxySession_status' - Undocumented member.
--
-- 'updatedTimestamp', 'proxySession_updatedTimestamp' - Undocumented member.
--
-- 'voiceConnectorId', 'proxySession_voiceConnectorId' - Undocumented member.
newProxySession ::
  ProxySession
newProxySession =
  ProxySession'
    { capabilities = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing,
      endedTimestamp = Prelude.Nothing,
      expiryMinutes = Prelude.Nothing,
      geoMatchLevel = Prelude.Nothing,
      geoMatchParams = Prelude.Nothing,
      name = Prelude.Nothing,
      numberSelectionBehavior = Prelude.Nothing,
      participants = Prelude.Nothing,
      proxySessionId = Prelude.Nothing,
      status = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing,
      voiceConnectorId = Prelude.Nothing
    }

-- | Undocumented member.
proxySession_capabilities :: Lens.Lens' ProxySession (Prelude.Maybe [Capability])
proxySession_capabilities = Lens.lens (\ProxySession' {capabilities} -> capabilities) (\s@ProxySession' {} a -> s {capabilities = a} :: ProxySession) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
proxySession_createdTimestamp :: Lens.Lens' ProxySession (Prelude.Maybe Prelude.UTCTime)
proxySession_createdTimestamp = Lens.lens (\ProxySession' {createdTimestamp} -> createdTimestamp) (\s@ProxySession' {} a -> s {createdTimestamp = a} :: ProxySession) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
proxySession_endedTimestamp :: Lens.Lens' ProxySession (Prelude.Maybe Prelude.UTCTime)
proxySession_endedTimestamp = Lens.lens (\ProxySession' {endedTimestamp} -> endedTimestamp) (\s@ProxySession' {} a -> s {endedTimestamp = a} :: ProxySession) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
proxySession_expiryMinutes :: Lens.Lens' ProxySession (Prelude.Maybe Prelude.Natural)
proxySession_expiryMinutes = Lens.lens (\ProxySession' {expiryMinutes} -> expiryMinutes) (\s@ProxySession' {} a -> s {expiryMinutes = a} :: ProxySession)

-- | Undocumented member.
proxySession_geoMatchLevel :: Lens.Lens' ProxySession (Prelude.Maybe GeoMatchLevel)
proxySession_geoMatchLevel = Lens.lens (\ProxySession' {geoMatchLevel} -> geoMatchLevel) (\s@ProxySession' {} a -> s {geoMatchLevel = a} :: ProxySession)

-- | Undocumented member.
proxySession_geoMatchParams :: Lens.Lens' ProxySession (Prelude.Maybe GeoMatchParams)
proxySession_geoMatchParams = Lens.lens (\ProxySession' {geoMatchParams} -> geoMatchParams) (\s@ProxySession' {} a -> s {geoMatchParams = a} :: ProxySession)

-- | Undocumented member.
proxySession_name :: Lens.Lens' ProxySession (Prelude.Maybe Prelude.Text)
proxySession_name = Lens.lens (\ProxySession' {name} -> name) (\s@ProxySession' {} a -> s {name = a} :: ProxySession)

-- | Undocumented member.
proxySession_numberSelectionBehavior :: Lens.Lens' ProxySession (Prelude.Maybe NumberSelectionBehavior)
proxySession_numberSelectionBehavior = Lens.lens (\ProxySession' {numberSelectionBehavior} -> numberSelectionBehavior) (\s@ProxySession' {} a -> s {numberSelectionBehavior = a} :: ProxySession)

-- | Undocumented member.
proxySession_participants :: Lens.Lens' ProxySession (Prelude.Maybe [Participant])
proxySession_participants = Lens.lens (\ProxySession' {participants} -> participants) (\s@ProxySession' {} a -> s {participants = a} :: ProxySession) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
proxySession_proxySessionId :: Lens.Lens' ProxySession (Prelude.Maybe Prelude.Text)
proxySession_proxySessionId = Lens.lens (\ProxySession' {proxySessionId} -> proxySessionId) (\s@ProxySession' {} a -> s {proxySessionId = a} :: ProxySession)

-- | Undocumented member.
proxySession_status :: Lens.Lens' ProxySession (Prelude.Maybe ProxySessionStatus)
proxySession_status = Lens.lens (\ProxySession' {status} -> status) (\s@ProxySession' {} a -> s {status = a} :: ProxySession)

-- | Undocumented member.
proxySession_updatedTimestamp :: Lens.Lens' ProxySession (Prelude.Maybe Prelude.UTCTime)
proxySession_updatedTimestamp = Lens.lens (\ProxySession' {updatedTimestamp} -> updatedTimestamp) (\s@ProxySession' {} a -> s {updatedTimestamp = a} :: ProxySession) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
proxySession_voiceConnectorId :: Lens.Lens' ProxySession (Prelude.Maybe Prelude.Text)
proxySession_voiceConnectorId = Lens.lens (\ProxySession' {voiceConnectorId} -> voiceConnectorId) (\s@ProxySession' {} a -> s {voiceConnectorId = a} :: ProxySession)

instance Data.FromJSON ProxySession where
  parseJSON =
    Data.withObject
      "ProxySession"
      ( \x ->
          ProxySession'
            Prelude.<$> (x Data..:? "Capabilities" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "EndedTimestamp")
            Prelude.<*> (x Data..:? "ExpiryMinutes")
            Prelude.<*> (x Data..:? "GeoMatchLevel")
            Prelude.<*> (x Data..:? "GeoMatchParams")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "NumberSelectionBehavior")
            Prelude.<*> (x Data..:? "Participants" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ProxySessionId")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "UpdatedTimestamp")
            Prelude.<*> (x Data..:? "VoiceConnectorId")
      )

instance Prelude.Hashable ProxySession where
  hashWithSalt _salt ProxySession' {..} =
    _salt
      `Prelude.hashWithSalt` capabilities
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` endedTimestamp
      `Prelude.hashWithSalt` expiryMinutes
      `Prelude.hashWithSalt` geoMatchLevel
      `Prelude.hashWithSalt` geoMatchParams
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` numberSelectionBehavior
      `Prelude.hashWithSalt` participants
      `Prelude.hashWithSalt` proxySessionId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` updatedTimestamp
      `Prelude.hashWithSalt` voiceConnectorId

instance Prelude.NFData ProxySession where
  rnf ProxySession' {..} =
    Prelude.rnf capabilities
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf endedTimestamp
      `Prelude.seq` Prelude.rnf expiryMinutes
      `Prelude.seq` Prelude.rnf geoMatchLevel
      `Prelude.seq` Prelude.rnf geoMatchParams
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf numberSelectionBehavior
      `Prelude.seq` Prelude.rnf participants
      `Prelude.seq` Prelude.rnf proxySessionId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updatedTimestamp
      `Prelude.seq` Prelude.rnf voiceConnectorId
