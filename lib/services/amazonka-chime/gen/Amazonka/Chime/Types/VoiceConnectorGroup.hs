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
-- Module      : Amazonka.Chime.Types.VoiceConnectorGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.VoiceConnectorGroup where

import Amazonka.Chime.Types.VoiceConnectorItem
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Chime Voice Connector group configuration, including
-- associated Amazon Chime Voice Connectors. You can include Amazon Chime
-- Voice Connectors from different AWS Regions in your group. This creates
-- a fault tolerant mechanism for fallback in case of availability events.
--
-- /See:/ 'newVoiceConnectorGroup' smart constructor.
data VoiceConnectorGroup = VoiceConnectorGroup'
  { -- | The Amazon Chime Voice Connector group ID.
    voiceConnectorGroupId :: Prelude.Maybe Prelude.Text,
    -- | The updated Amazon Chime Voice Connector group time stamp, in ISO 8601
    -- format.
    updatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Chime Voice Connectors to which to route inbound calls.
    voiceConnectorItems :: Prelude.Maybe [VoiceConnectorItem],
    -- | The ARN of the specified Amazon Chime Voice Connector group.
    voiceConnectorGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon Chime Voice Connector group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Chime Voice Connector group creation time stamp, in ISO 8601
    -- format.
    createdTimestamp :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VoiceConnectorGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnectorGroupId', 'voiceConnectorGroup_voiceConnectorGroupId' - The Amazon Chime Voice Connector group ID.
--
-- 'updatedTimestamp', 'voiceConnectorGroup_updatedTimestamp' - The updated Amazon Chime Voice Connector group time stamp, in ISO 8601
-- format.
--
-- 'voiceConnectorItems', 'voiceConnectorGroup_voiceConnectorItems' - The Amazon Chime Voice Connectors to which to route inbound calls.
--
-- 'voiceConnectorGroupArn', 'voiceConnectorGroup_voiceConnectorGroupArn' - The ARN of the specified Amazon Chime Voice Connector group.
--
-- 'name', 'voiceConnectorGroup_name' - The name of the Amazon Chime Voice Connector group.
--
-- 'createdTimestamp', 'voiceConnectorGroup_createdTimestamp' - The Amazon Chime Voice Connector group creation time stamp, in ISO 8601
-- format.
newVoiceConnectorGroup ::
  VoiceConnectorGroup
newVoiceConnectorGroup =
  VoiceConnectorGroup'
    { voiceConnectorGroupId =
        Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing,
      voiceConnectorItems = Prelude.Nothing,
      voiceConnectorGroupArn = Prelude.Nothing,
      name = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing
    }

-- | The Amazon Chime Voice Connector group ID.
voiceConnectorGroup_voiceConnectorGroupId :: Lens.Lens' VoiceConnectorGroup (Prelude.Maybe Prelude.Text)
voiceConnectorGroup_voiceConnectorGroupId = Lens.lens (\VoiceConnectorGroup' {voiceConnectorGroupId} -> voiceConnectorGroupId) (\s@VoiceConnectorGroup' {} a -> s {voiceConnectorGroupId = a} :: VoiceConnectorGroup)

-- | The updated Amazon Chime Voice Connector group time stamp, in ISO 8601
-- format.
voiceConnectorGroup_updatedTimestamp :: Lens.Lens' VoiceConnectorGroup (Prelude.Maybe Prelude.UTCTime)
voiceConnectorGroup_updatedTimestamp = Lens.lens (\VoiceConnectorGroup' {updatedTimestamp} -> updatedTimestamp) (\s@VoiceConnectorGroup' {} a -> s {updatedTimestamp = a} :: VoiceConnectorGroup) Prelude.. Lens.mapping Core._Time

-- | The Amazon Chime Voice Connectors to which to route inbound calls.
voiceConnectorGroup_voiceConnectorItems :: Lens.Lens' VoiceConnectorGroup (Prelude.Maybe [VoiceConnectorItem])
voiceConnectorGroup_voiceConnectorItems = Lens.lens (\VoiceConnectorGroup' {voiceConnectorItems} -> voiceConnectorItems) (\s@VoiceConnectorGroup' {} a -> s {voiceConnectorItems = a} :: VoiceConnectorGroup) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the specified Amazon Chime Voice Connector group.
voiceConnectorGroup_voiceConnectorGroupArn :: Lens.Lens' VoiceConnectorGroup (Prelude.Maybe Prelude.Text)
voiceConnectorGroup_voiceConnectorGroupArn = Lens.lens (\VoiceConnectorGroup' {voiceConnectorGroupArn} -> voiceConnectorGroupArn) (\s@VoiceConnectorGroup' {} a -> s {voiceConnectorGroupArn = a} :: VoiceConnectorGroup)

-- | The name of the Amazon Chime Voice Connector group.
voiceConnectorGroup_name :: Lens.Lens' VoiceConnectorGroup (Prelude.Maybe Prelude.Text)
voiceConnectorGroup_name = Lens.lens (\VoiceConnectorGroup' {name} -> name) (\s@VoiceConnectorGroup' {} a -> s {name = a} :: VoiceConnectorGroup)

-- | The Amazon Chime Voice Connector group creation time stamp, in ISO 8601
-- format.
voiceConnectorGroup_createdTimestamp :: Lens.Lens' VoiceConnectorGroup (Prelude.Maybe Prelude.UTCTime)
voiceConnectorGroup_createdTimestamp = Lens.lens (\VoiceConnectorGroup' {createdTimestamp} -> createdTimestamp) (\s@VoiceConnectorGroup' {} a -> s {createdTimestamp = a} :: VoiceConnectorGroup) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON VoiceConnectorGroup where
  parseJSON =
    Core.withObject
      "VoiceConnectorGroup"
      ( \x ->
          VoiceConnectorGroup'
            Prelude.<$> (x Core..:? "VoiceConnectorGroupId")
            Prelude.<*> (x Core..:? "UpdatedTimestamp")
            Prelude.<*> ( x Core..:? "VoiceConnectorItems"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "VoiceConnectorGroupArn")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
      )

instance Prelude.Hashable VoiceConnectorGroup where
  hashWithSalt salt' VoiceConnectorGroup' {..} =
    salt' `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` voiceConnectorGroupArn
      `Prelude.hashWithSalt` voiceConnectorItems
      `Prelude.hashWithSalt` updatedTimestamp
      `Prelude.hashWithSalt` voiceConnectorGroupId

instance Prelude.NFData VoiceConnectorGroup where
  rnf VoiceConnectorGroup' {..} =
    Prelude.rnf voiceConnectorGroupId
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf voiceConnectorGroupArn
      `Prelude.seq` Prelude.rnf voiceConnectorItems
      `Prelude.seq` Prelude.rnf updatedTimestamp
