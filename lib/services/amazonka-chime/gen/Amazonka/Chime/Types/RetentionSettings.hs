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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.RetentionSettings where

import Amazonka.Chime.Types.ConversationRetentionSettings
import Amazonka.Chime.Types.RoomRetentionSettings
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The retention settings for an Amazon Chime Enterprise account that
-- determine how long to retain items such as chat-room messages and
-- chat-conversation messages.
--
-- /See:/ 'newRetentionSettings' smart constructor.
data RetentionSettings = RetentionSettings'
  { -- | The chat conversation retention settings.
    conversationRetentionSettings :: Prelude.Maybe ConversationRetentionSettings,
    -- | The chat room retention settings.
    roomRetentionSettings :: Prelude.Maybe RoomRetentionSettings
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
-- 'conversationRetentionSettings', 'retentionSettings_conversationRetentionSettings' - The chat conversation retention settings.
--
-- 'roomRetentionSettings', 'retentionSettings_roomRetentionSettings' - The chat room retention settings.
newRetentionSettings ::
  RetentionSettings
newRetentionSettings =
  RetentionSettings'
    { conversationRetentionSettings =
        Prelude.Nothing,
      roomRetentionSettings = Prelude.Nothing
    }

-- | The chat conversation retention settings.
retentionSettings_conversationRetentionSettings :: Lens.Lens' RetentionSettings (Prelude.Maybe ConversationRetentionSettings)
retentionSettings_conversationRetentionSettings = Lens.lens (\RetentionSettings' {conversationRetentionSettings} -> conversationRetentionSettings) (\s@RetentionSettings' {} a -> s {conversationRetentionSettings = a} :: RetentionSettings)

-- | The chat room retention settings.
retentionSettings_roomRetentionSettings :: Lens.Lens' RetentionSettings (Prelude.Maybe RoomRetentionSettings)
retentionSettings_roomRetentionSettings = Lens.lens (\RetentionSettings' {roomRetentionSettings} -> roomRetentionSettings) (\s@RetentionSettings' {} a -> s {roomRetentionSettings = a} :: RetentionSettings)

instance Data.FromJSON RetentionSettings where
  parseJSON =
    Data.withObject
      "RetentionSettings"
      ( \x ->
          RetentionSettings'
            Prelude.<$> (x Data..:? "ConversationRetentionSettings")
            Prelude.<*> (x Data..:? "RoomRetentionSettings")
      )

instance Prelude.Hashable RetentionSettings where
  hashWithSalt _salt RetentionSettings' {..} =
    _salt
      `Prelude.hashWithSalt` conversationRetentionSettings
      `Prelude.hashWithSalt` roomRetentionSettings

instance Prelude.NFData RetentionSettings where
  rnf RetentionSettings' {..} =
    Prelude.rnf conversationRetentionSettings
      `Prelude.seq` Prelude.rnf roomRetentionSettings

instance Data.ToJSON RetentionSettings where
  toJSON RetentionSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConversationRetentionSettings" Data..=)
              Prelude.<$> conversationRetentionSettings,
            ("RoomRetentionSettings" Data..=)
              Prelude.<$> roomRetentionSettings
          ]
      )
