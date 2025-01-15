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
-- Module      : Amazonka.Nimble.Types.StreamConfigurationSessionBackup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.StreamConfigurationSessionBackup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types.SessionBackupMode
import qualified Amazonka.Prelude as Prelude

-- | Configures how streaming sessions are backed up when launched from this
-- launch profile.
--
-- /See:/ 'newStreamConfigurationSessionBackup' smart constructor.
data StreamConfigurationSessionBackup = StreamConfigurationSessionBackup'
  { -- | The maximum number of backups that each streaming session created from
    -- this launch profile can have.
    maxBackupsToRetain :: Prelude.Maybe Prelude.Natural,
    -- | Specifies how artists sessions are backed up.
    --
    -- Configures backups for streaming sessions launched with this launch
    -- profile. The default value is @DEACTIVATED@, which means that backups
    -- are deactivated. To allow backups, set this value to @AUTOMATIC@.
    mode :: Prelude.Maybe SessionBackupMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamConfigurationSessionBackup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxBackupsToRetain', 'streamConfigurationSessionBackup_maxBackupsToRetain' - The maximum number of backups that each streaming session created from
-- this launch profile can have.
--
-- 'mode', 'streamConfigurationSessionBackup_mode' - Specifies how artists sessions are backed up.
--
-- Configures backups for streaming sessions launched with this launch
-- profile. The default value is @DEACTIVATED@, which means that backups
-- are deactivated. To allow backups, set this value to @AUTOMATIC@.
newStreamConfigurationSessionBackup ::
  StreamConfigurationSessionBackup
newStreamConfigurationSessionBackup =
  StreamConfigurationSessionBackup'
    { maxBackupsToRetain =
        Prelude.Nothing,
      mode = Prelude.Nothing
    }

-- | The maximum number of backups that each streaming session created from
-- this launch profile can have.
streamConfigurationSessionBackup_maxBackupsToRetain :: Lens.Lens' StreamConfigurationSessionBackup (Prelude.Maybe Prelude.Natural)
streamConfigurationSessionBackup_maxBackupsToRetain = Lens.lens (\StreamConfigurationSessionBackup' {maxBackupsToRetain} -> maxBackupsToRetain) (\s@StreamConfigurationSessionBackup' {} a -> s {maxBackupsToRetain = a} :: StreamConfigurationSessionBackup)

-- | Specifies how artists sessions are backed up.
--
-- Configures backups for streaming sessions launched with this launch
-- profile. The default value is @DEACTIVATED@, which means that backups
-- are deactivated. To allow backups, set this value to @AUTOMATIC@.
streamConfigurationSessionBackup_mode :: Lens.Lens' StreamConfigurationSessionBackup (Prelude.Maybe SessionBackupMode)
streamConfigurationSessionBackup_mode = Lens.lens (\StreamConfigurationSessionBackup' {mode} -> mode) (\s@StreamConfigurationSessionBackup' {} a -> s {mode = a} :: StreamConfigurationSessionBackup)

instance
  Data.FromJSON
    StreamConfigurationSessionBackup
  where
  parseJSON =
    Data.withObject
      "StreamConfigurationSessionBackup"
      ( \x ->
          StreamConfigurationSessionBackup'
            Prelude.<$> (x Data..:? "maxBackupsToRetain")
            Prelude.<*> (x Data..:? "mode")
      )

instance
  Prelude.Hashable
    StreamConfigurationSessionBackup
  where
  hashWithSalt
    _salt
    StreamConfigurationSessionBackup' {..} =
      _salt
        `Prelude.hashWithSalt` maxBackupsToRetain
        `Prelude.hashWithSalt` mode

instance
  Prelude.NFData
    StreamConfigurationSessionBackup
  where
  rnf StreamConfigurationSessionBackup' {..} =
    Prelude.rnf maxBackupsToRetain `Prelude.seq`
      Prelude.rnf mode

instance Data.ToJSON StreamConfigurationSessionBackup where
  toJSON StreamConfigurationSessionBackup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxBackupsToRetain" Data..=)
              Prelude.<$> maxBackupsToRetain,
            ("mode" Data..=) Prelude.<$> mode
          ]
      )
