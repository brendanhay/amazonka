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
-- Module      : Amazonka.ChimeSdkVoice.Types.VoiceConnectorGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.VoiceConnectorGroup where

import Amazonka.ChimeSdkVoice.Types.VoiceConnectorItem
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newVoiceConnectorGroup' smart constructor.
data VoiceConnectorGroup = VoiceConnectorGroup'
  { createdTimestamp :: Prelude.Maybe Data.ISO8601,
    name :: Prelude.Maybe Prelude.Text,
    updatedTimestamp :: Prelude.Maybe Data.ISO8601,
    voiceConnectorGroupArn :: Prelude.Maybe Prelude.Text,
    voiceConnectorGroupId :: Prelude.Maybe Prelude.Text,
    voiceConnectorItems :: Prelude.Maybe [VoiceConnectorItem]
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
-- 'createdTimestamp', 'voiceConnectorGroup_createdTimestamp' - Undocumented member.
--
-- 'name', 'voiceConnectorGroup_name' - Undocumented member.
--
-- 'updatedTimestamp', 'voiceConnectorGroup_updatedTimestamp' - Undocumented member.
--
-- 'voiceConnectorGroupArn', 'voiceConnectorGroup_voiceConnectorGroupArn' - Undocumented member.
--
-- 'voiceConnectorGroupId', 'voiceConnectorGroup_voiceConnectorGroupId' - Undocumented member.
--
-- 'voiceConnectorItems', 'voiceConnectorGroup_voiceConnectorItems' - Undocumented member.
newVoiceConnectorGroup ::
  VoiceConnectorGroup
newVoiceConnectorGroup =
  VoiceConnectorGroup'
    { createdTimestamp =
        Prelude.Nothing,
      name = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing,
      voiceConnectorGroupArn = Prelude.Nothing,
      voiceConnectorGroupId = Prelude.Nothing,
      voiceConnectorItems = Prelude.Nothing
    }

-- | Undocumented member.
voiceConnectorGroup_createdTimestamp :: Lens.Lens' VoiceConnectorGroup (Prelude.Maybe Prelude.UTCTime)
voiceConnectorGroup_createdTimestamp = Lens.lens (\VoiceConnectorGroup' {createdTimestamp} -> createdTimestamp) (\s@VoiceConnectorGroup' {} a -> s {createdTimestamp = a} :: VoiceConnectorGroup) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
voiceConnectorGroup_name :: Lens.Lens' VoiceConnectorGroup (Prelude.Maybe Prelude.Text)
voiceConnectorGroup_name = Lens.lens (\VoiceConnectorGroup' {name} -> name) (\s@VoiceConnectorGroup' {} a -> s {name = a} :: VoiceConnectorGroup)

-- | Undocumented member.
voiceConnectorGroup_updatedTimestamp :: Lens.Lens' VoiceConnectorGroup (Prelude.Maybe Prelude.UTCTime)
voiceConnectorGroup_updatedTimestamp = Lens.lens (\VoiceConnectorGroup' {updatedTimestamp} -> updatedTimestamp) (\s@VoiceConnectorGroup' {} a -> s {updatedTimestamp = a} :: VoiceConnectorGroup) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
voiceConnectorGroup_voiceConnectorGroupArn :: Lens.Lens' VoiceConnectorGroup (Prelude.Maybe Prelude.Text)
voiceConnectorGroup_voiceConnectorGroupArn = Lens.lens (\VoiceConnectorGroup' {voiceConnectorGroupArn} -> voiceConnectorGroupArn) (\s@VoiceConnectorGroup' {} a -> s {voiceConnectorGroupArn = a} :: VoiceConnectorGroup)

-- | Undocumented member.
voiceConnectorGroup_voiceConnectorGroupId :: Lens.Lens' VoiceConnectorGroup (Prelude.Maybe Prelude.Text)
voiceConnectorGroup_voiceConnectorGroupId = Lens.lens (\VoiceConnectorGroup' {voiceConnectorGroupId} -> voiceConnectorGroupId) (\s@VoiceConnectorGroup' {} a -> s {voiceConnectorGroupId = a} :: VoiceConnectorGroup)

-- | Undocumented member.
voiceConnectorGroup_voiceConnectorItems :: Lens.Lens' VoiceConnectorGroup (Prelude.Maybe [VoiceConnectorItem])
voiceConnectorGroup_voiceConnectorItems = Lens.lens (\VoiceConnectorGroup' {voiceConnectorItems} -> voiceConnectorItems) (\s@VoiceConnectorGroup' {} a -> s {voiceConnectorItems = a} :: VoiceConnectorGroup) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON VoiceConnectorGroup where
  parseJSON =
    Data.withObject
      "VoiceConnectorGroup"
      ( \x ->
          VoiceConnectorGroup'
            Prelude.<$> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "UpdatedTimestamp")
            Prelude.<*> (x Data..:? "VoiceConnectorGroupArn")
            Prelude.<*> (x Data..:? "VoiceConnectorGroupId")
            Prelude.<*> ( x
                            Data..:? "VoiceConnectorItems"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable VoiceConnectorGroup where
  hashWithSalt _salt VoiceConnectorGroup' {..} =
    _salt
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` updatedTimestamp
      `Prelude.hashWithSalt` voiceConnectorGroupArn
      `Prelude.hashWithSalt` voiceConnectorGroupId
      `Prelude.hashWithSalt` voiceConnectorItems

instance Prelude.NFData VoiceConnectorGroup where
  rnf VoiceConnectorGroup' {..} =
    Prelude.rnf createdTimestamp `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf updatedTimestamp `Prelude.seq`
          Prelude.rnf voiceConnectorGroupArn `Prelude.seq`
            Prelude.rnf voiceConnectorGroupId `Prelude.seq`
              Prelude.rnf voiceConnectorItems
