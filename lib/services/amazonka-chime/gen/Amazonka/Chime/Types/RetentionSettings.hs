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
-- Module      : Amazonka.Chime.Types.RetentionSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.RetentionSettings where

import Amazonka.Chime.Types.ConversationRetentionSettings
import Amazonka.Chime.Types.RoomRetentionSettings
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The retention settings for an Amazon Chime Enterprise account that
-- determine how long to retain items such as chat-room messages and
-- chat-conversation messages.
--
-- /See:/ 'newRetentionSettings' smart constructor.
data RetentionSettings = RetentionSettings'
  { -- | The chat room retention settings.
    roomRetentionSettings :: Prelude.Maybe RoomRetentionSettings,
    -- | The chat conversation retention settings.
    conversationRetentionSettings :: Prelude.Maybe ConversationRetentionSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetentionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roomRetentionSettings', 'retentionSettings_roomRetentionSettings' - The chat room retention settings.
--
-- 'conversationRetentionSettings', 'retentionSettings_conversationRetentionSettings' - The chat conversation retention settings.
newRetentionSettings ::
  RetentionSettings
newRetentionSettings =
  RetentionSettings'
    { roomRetentionSettings =
        Prelude.Nothing,
      conversationRetentionSettings = Prelude.Nothing
    }

-- | The chat room retention settings.
retentionSettings_roomRetentionSettings :: Lens.Lens' RetentionSettings (Prelude.Maybe RoomRetentionSettings)
retentionSettings_roomRetentionSettings = Lens.lens (\RetentionSettings' {roomRetentionSettings} -> roomRetentionSettings) (\s@RetentionSettings' {} a -> s {roomRetentionSettings = a} :: RetentionSettings)

-- | The chat conversation retention settings.
retentionSettings_conversationRetentionSettings :: Lens.Lens' RetentionSettings (Prelude.Maybe ConversationRetentionSettings)
retentionSettings_conversationRetentionSettings = Lens.lens (\RetentionSettings' {conversationRetentionSettings} -> conversationRetentionSettings) (\s@RetentionSettings' {} a -> s {conversationRetentionSettings = a} :: RetentionSettings)

instance Core.FromJSON RetentionSettings where
  parseJSON =
    Core.withObject
      "RetentionSettings"
      ( \x ->
          RetentionSettings'
            Prelude.<$> (x Core..:? "RoomRetentionSettings")
            Prelude.<*> (x Core..:? "ConversationRetentionSettings")
      )

instance Prelude.Hashable RetentionSettings where
  hashWithSalt _salt RetentionSettings' {..} =
    _salt `Prelude.hashWithSalt` roomRetentionSettings
      `Prelude.hashWithSalt` conversationRetentionSettings

instance Prelude.NFData RetentionSettings where
  rnf RetentionSettings' {..} =
    Prelude.rnf roomRetentionSettings
      `Prelude.seq` Prelude.rnf conversationRetentionSettings

instance Core.ToJSON RetentionSettings where
  toJSON RetentionSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RoomRetentionSettings" Core..=)
              Prelude.<$> roomRetentionSettings,
            ("ConversationRetentionSettings" Core..=)
              Prelude.<$> conversationRetentionSettings
          ]
      )
