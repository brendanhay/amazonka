{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateErrorItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateErrorItem where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.EnableFastSnapshotRestoreStateError
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about an error that occurred when enabling fast
-- snapshot restores.
--
-- /See:/ 'newEnableFastSnapshotRestoreStateErrorItem' smart constructor.
data EnableFastSnapshotRestoreStateErrorItem = EnableFastSnapshotRestoreStateErrorItem'
  { -- | The Availability Zone.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The error.
    error :: Prelude.Maybe EnableFastSnapshotRestoreStateError
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnableFastSnapshotRestoreStateErrorItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'enableFastSnapshotRestoreStateErrorItem_availabilityZone' - The Availability Zone.
--
-- 'error', 'enableFastSnapshotRestoreStateErrorItem_error' - The error.
newEnableFastSnapshotRestoreStateErrorItem ::
  EnableFastSnapshotRestoreStateErrorItem
newEnableFastSnapshotRestoreStateErrorItem =
  EnableFastSnapshotRestoreStateErrorItem'
    { availabilityZone =
        Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | The Availability Zone.
enableFastSnapshotRestoreStateErrorItem_availabilityZone :: Lens.Lens' EnableFastSnapshotRestoreStateErrorItem (Prelude.Maybe Prelude.Text)
enableFastSnapshotRestoreStateErrorItem_availabilityZone = Lens.lens (\EnableFastSnapshotRestoreStateErrorItem' {availabilityZone} -> availabilityZone) (\s@EnableFastSnapshotRestoreStateErrorItem' {} a -> s {availabilityZone = a} :: EnableFastSnapshotRestoreStateErrorItem)

-- | The error.
enableFastSnapshotRestoreStateErrorItem_error :: Lens.Lens' EnableFastSnapshotRestoreStateErrorItem (Prelude.Maybe EnableFastSnapshotRestoreStateError)
enableFastSnapshotRestoreStateErrorItem_error = Lens.lens (\EnableFastSnapshotRestoreStateErrorItem' {error} -> error) (\s@EnableFastSnapshotRestoreStateErrorItem' {} a -> s {error = a} :: EnableFastSnapshotRestoreStateErrorItem)

instance
  Prelude.FromXML
    EnableFastSnapshotRestoreStateErrorItem
  where
  parseXML x =
    EnableFastSnapshotRestoreStateErrorItem'
      Prelude.<$> (x Prelude..@? "availabilityZone")
        Prelude.<*> (x Prelude..@? "error")

instance
  Prelude.Hashable
    EnableFastSnapshotRestoreStateErrorItem

instance
  Prelude.NFData
    EnableFastSnapshotRestoreStateErrorItem
