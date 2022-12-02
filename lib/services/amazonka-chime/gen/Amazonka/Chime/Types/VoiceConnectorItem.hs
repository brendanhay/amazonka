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
-- Module      : Amazonka.Chime.Types.VoiceConnectorItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.VoiceConnectorItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | For Amazon Chime Voice Connector groups, the Amazon Chime Voice
-- Connectors to which to route inbound calls. Includes priority
-- configuration settings. Limit: 3 @VoiceConnectorItems@ per Amazon Chime
-- Voice Connector group.
--
-- /See:/ 'newVoiceConnectorItem' smart constructor.
data VoiceConnectorItem = VoiceConnectorItem'
  { -- | The Amazon Chime Voice Connector ID.
    voiceConnectorId :: Prelude.Text,
    -- | The priority associated with the Amazon Chime Voice Connector, with 1
    -- being the highest priority. Higher priority Amazon Chime Voice
    -- Connectors are attempted first.
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
-- 'voiceConnectorId', 'voiceConnectorItem_voiceConnectorId' - The Amazon Chime Voice Connector ID.
--
-- 'priority', 'voiceConnectorItem_priority' - The priority associated with the Amazon Chime Voice Connector, with 1
-- being the highest priority. Higher priority Amazon Chime Voice
-- Connectors are attempted first.
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

-- | The Amazon Chime Voice Connector ID.
voiceConnectorItem_voiceConnectorId :: Lens.Lens' VoiceConnectorItem Prelude.Text
voiceConnectorItem_voiceConnectorId = Lens.lens (\VoiceConnectorItem' {voiceConnectorId} -> voiceConnectorId) (\s@VoiceConnectorItem' {} a -> s {voiceConnectorId = a} :: VoiceConnectorItem)

-- | The priority associated with the Amazon Chime Voice Connector, with 1
-- being the highest priority. Higher priority Amazon Chime Voice
-- Connectors are attempted first.
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
    _salt `Prelude.hashWithSalt` voiceConnectorId
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
