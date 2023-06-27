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
-- Module      : Amazonka.ChimeSdkVoice.Types.VoiceConnectorItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.VoiceConnectorItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | For Amazon Chime SDK Voice Connector groups, the Amazon Chime SDK Voice
-- Connectors to which you route inbound calls. Includes priority
-- configuration settings. Limit: 3 VoiceConnectorItems per Voice Connector
-- group.
--
-- /See:/ 'newVoiceConnectorItem' smart constructor.
data VoiceConnectorItem = VoiceConnectorItem'
  { -- | The Voice Connector ID.
    voiceConnectorId :: Prelude.Text,
    -- | The priority setting of a Voice Connector item. Calls are routed to
    -- hosts in priority order, with 1 as the highest priority. When hosts have
    -- equal priority, the system distributes calls among them based on their
    -- relative weight.
    priority :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VoiceConnectorItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorId', 'voiceConnectorItem_voiceConnectorId' - The Voice Connector ID.
--
-- 'priority', 'voiceConnectorItem_priority' - The priority setting of a Voice Connector item. Calls are routed to
-- hosts in priority order, with 1 as the highest priority. When hosts have
-- equal priority, the system distributes calls among them based on their
-- relative weight.
newVoiceConnectorItem ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  -- | 'priority'
  Prelude.Natural ->
  VoiceConnectorItem
newVoiceConnectorItem pVoiceConnectorId_ pPriority_ =
  VoiceConnectorItem'
    { voiceConnectorId =
        pVoiceConnectorId_,
      priority = pPriority_
    }

-- | The Voice Connector ID.
voiceConnectorItem_voiceConnectorId :: Lens.Lens' VoiceConnectorItem Prelude.Text
voiceConnectorItem_voiceConnectorId = Lens.lens (\VoiceConnectorItem' {voiceConnectorId} -> voiceConnectorId) (\s@VoiceConnectorItem' {} a -> s {voiceConnectorId = a} :: VoiceConnectorItem)

-- | The priority setting of a Voice Connector item. Calls are routed to
-- hosts in priority order, with 1 as the highest priority. When hosts have
-- equal priority, the system distributes calls among them based on their
-- relative weight.
voiceConnectorItem_priority :: Lens.Lens' VoiceConnectorItem Prelude.Natural
voiceConnectorItem_priority = Lens.lens (\VoiceConnectorItem' {priority} -> priority) (\s@VoiceConnectorItem' {} a -> s {priority = a} :: VoiceConnectorItem)

instance Data.FromJSON VoiceConnectorItem where
  parseJSON =
    Data.withObject
      "VoiceConnectorItem"
      ( \x ->
          VoiceConnectorItem'
            Prelude.<$> (x Data..: "VoiceConnectorId")
            Prelude.<*> (x Data..: "Priority")
      )

instance Prelude.Hashable VoiceConnectorItem where
  hashWithSalt _salt VoiceConnectorItem' {..} =
    _salt
      `Prelude.hashWithSalt` voiceConnectorId
      `Prelude.hashWithSalt` priority

instance Prelude.NFData VoiceConnectorItem where
  rnf VoiceConnectorItem' {..} =
    Prelude.rnf voiceConnectorId
      `Prelude.seq` Prelude.rnf priority

instance Data.ToJSON VoiceConnectorItem where
  toJSON VoiceConnectorItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("VoiceConnectorId" Data..= voiceConnectorId),
            Prelude.Just ("Priority" Data..= priority)
          ]
      )
