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
-- Module      : Amazonka.DrS.Types.StartRecoveryRequestSourceServer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.StartRecoveryRequestSourceServer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object representing the Source Server to recover.
--
-- /See:/ 'newStartRecoveryRequestSourceServer' smart constructor.
data StartRecoveryRequestSourceServer = StartRecoveryRequestSourceServer'
  { -- | The ID of a Recovery Snapshot we want to recover from. Omit this field
    -- to launch from the latest data by taking an on-demand snapshot.
    recoverySnapshotID :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Source Server you want to recover.
    sourceServerID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartRecoveryRequestSourceServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recoverySnapshotID', 'startRecoveryRequestSourceServer_recoverySnapshotID' - The ID of a Recovery Snapshot we want to recover from. Omit this field
-- to launch from the latest data by taking an on-demand snapshot.
--
-- 'sourceServerID', 'startRecoveryRequestSourceServer_sourceServerID' - The ID of the Source Server you want to recover.
newStartRecoveryRequestSourceServer ::
  -- | 'sourceServerID'
  Prelude.Text ->
  StartRecoveryRequestSourceServer
newStartRecoveryRequestSourceServer pSourceServerID_ =
  StartRecoveryRequestSourceServer'
    { recoverySnapshotID =
        Prelude.Nothing,
      sourceServerID = pSourceServerID_
    }

-- | The ID of a Recovery Snapshot we want to recover from. Omit this field
-- to launch from the latest data by taking an on-demand snapshot.
startRecoveryRequestSourceServer_recoverySnapshotID :: Lens.Lens' StartRecoveryRequestSourceServer (Prelude.Maybe Prelude.Text)
startRecoveryRequestSourceServer_recoverySnapshotID = Lens.lens (\StartRecoveryRequestSourceServer' {recoverySnapshotID} -> recoverySnapshotID) (\s@StartRecoveryRequestSourceServer' {} a -> s {recoverySnapshotID = a} :: StartRecoveryRequestSourceServer)

-- | The ID of the Source Server you want to recover.
startRecoveryRequestSourceServer_sourceServerID :: Lens.Lens' StartRecoveryRequestSourceServer Prelude.Text
startRecoveryRequestSourceServer_sourceServerID = Lens.lens (\StartRecoveryRequestSourceServer' {sourceServerID} -> sourceServerID) (\s@StartRecoveryRequestSourceServer' {} a -> s {sourceServerID = a} :: StartRecoveryRequestSourceServer)

instance
  Prelude.Hashable
    StartRecoveryRequestSourceServer
  where
  hashWithSalt
    _salt
    StartRecoveryRequestSourceServer' {..} =
      _salt `Prelude.hashWithSalt` recoverySnapshotID
        `Prelude.hashWithSalt` sourceServerID

instance
  Prelude.NFData
    StartRecoveryRequestSourceServer
  where
  rnf StartRecoveryRequestSourceServer' {..} =
    Prelude.rnf recoverySnapshotID
      `Prelude.seq` Prelude.rnf sourceServerID

instance Data.ToJSON StartRecoveryRequestSourceServer where
  toJSON StartRecoveryRequestSourceServer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("recoverySnapshotID" Data..=)
              Prelude.<$> recoverySnapshotID,
            Prelude.Just
              ("sourceServerID" Data..= sourceServerID)
          ]
      )
