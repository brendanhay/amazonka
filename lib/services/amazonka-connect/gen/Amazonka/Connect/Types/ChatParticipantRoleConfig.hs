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
-- Module      : Amazonka.Connect.Types.ChatParticipantRoleConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ChatParticipantRoleConfig where

import Amazonka.Connect.Types.ParticipantTimerConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration information for the chat participant role.
--
-- /See:/ 'newChatParticipantRoleConfig' smart constructor.
data ChatParticipantRoleConfig = ChatParticipantRoleConfig'
  { -- | A list of participant timers. You can specify any unique combination of
    -- role and timer type. Duplicate entries error out the request with a 400.
    participantTimerConfigList :: Prelude.NonEmpty ParticipantTimerConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChatParticipantRoleConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'participantTimerConfigList', 'chatParticipantRoleConfig_participantTimerConfigList' - A list of participant timers. You can specify any unique combination of
-- role and timer type. Duplicate entries error out the request with a 400.
newChatParticipantRoleConfig ::
  -- | 'participantTimerConfigList'
  Prelude.NonEmpty ParticipantTimerConfiguration ->
  ChatParticipantRoleConfig
newChatParticipantRoleConfig
  pParticipantTimerConfigList_ =
    ChatParticipantRoleConfig'
      { participantTimerConfigList =
          Lens.coerced
            Lens.# pParticipantTimerConfigList_
      }

-- | A list of participant timers. You can specify any unique combination of
-- role and timer type. Duplicate entries error out the request with a 400.
chatParticipantRoleConfig_participantTimerConfigList :: Lens.Lens' ChatParticipantRoleConfig (Prelude.NonEmpty ParticipantTimerConfiguration)
chatParticipantRoleConfig_participantTimerConfigList = Lens.lens (\ChatParticipantRoleConfig' {participantTimerConfigList} -> participantTimerConfigList) (\s@ChatParticipantRoleConfig' {} a -> s {participantTimerConfigList = a} :: ChatParticipantRoleConfig) Prelude.. Lens.coerced

instance Prelude.Hashable ChatParticipantRoleConfig where
  hashWithSalt _salt ChatParticipantRoleConfig' {..} =
    _salt
      `Prelude.hashWithSalt` participantTimerConfigList

instance Prelude.NFData ChatParticipantRoleConfig where
  rnf ChatParticipantRoleConfig' {..} =
    Prelude.rnf participantTimerConfigList

instance Data.ToJSON ChatParticipantRoleConfig where
  toJSON ChatParticipantRoleConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ParticipantTimerConfigList"
                  Data..= participantTimerConfigList
              )
          ]
      )
