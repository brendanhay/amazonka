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
-- Module      : Network.AWS.Redshift.Types.DeleteClusterSnapshotMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.DeleteClusterSnapshotMessage where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

-- |
--
-- /See:/ 'newDeleteClusterSnapshotMessage' smart constructor.
data DeleteClusterSnapshotMessage = DeleteClusterSnapshotMessage'
  { -- | The unique identifier of the cluster the snapshot was created from. This
    -- parameter is required if your IAM user has a policy containing a
    -- snapshot resource element that specifies anything other than * for the
    -- cluster name.
    --
    -- Constraints: Must be the name of valid cluster.
    snapshotClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the manual snapshot to be deleted.
    --
    -- Constraints: Must be the name of an existing snapshot that is in the
    -- @available@, @failed@, or @cancelled@ state.
    snapshotIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteClusterSnapshotMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotClusterIdentifier', 'deleteClusterSnapshotMessage_snapshotClusterIdentifier' - The unique identifier of the cluster the snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a
-- snapshot resource element that specifies anything other than * for the
-- cluster name.
--
-- Constraints: Must be the name of valid cluster.
--
-- 'snapshotIdentifier', 'deleteClusterSnapshotMessage_snapshotIdentifier' - The unique identifier of the manual snapshot to be deleted.
--
-- Constraints: Must be the name of an existing snapshot that is in the
-- @available@, @failed@, or @cancelled@ state.
newDeleteClusterSnapshotMessage ::
  -- | 'snapshotIdentifier'
  Prelude.Text ->
  DeleteClusterSnapshotMessage
newDeleteClusterSnapshotMessage pSnapshotIdentifier_ =
  DeleteClusterSnapshotMessage'
    { snapshotClusterIdentifier =
        Prelude.Nothing,
      snapshotIdentifier = pSnapshotIdentifier_
    }

-- | The unique identifier of the cluster the snapshot was created from. This
-- parameter is required if your IAM user has a policy containing a
-- snapshot resource element that specifies anything other than * for the
-- cluster name.
--
-- Constraints: Must be the name of valid cluster.
deleteClusterSnapshotMessage_snapshotClusterIdentifier :: Lens.Lens' DeleteClusterSnapshotMessage (Prelude.Maybe Prelude.Text)
deleteClusterSnapshotMessage_snapshotClusterIdentifier = Lens.lens (\DeleteClusterSnapshotMessage' {snapshotClusterIdentifier} -> snapshotClusterIdentifier) (\s@DeleteClusterSnapshotMessage' {} a -> s {snapshotClusterIdentifier = a} :: DeleteClusterSnapshotMessage)

-- | The unique identifier of the manual snapshot to be deleted.
--
-- Constraints: Must be the name of an existing snapshot that is in the
-- @available@, @failed@, or @cancelled@ state.
deleteClusterSnapshotMessage_snapshotIdentifier :: Lens.Lens' DeleteClusterSnapshotMessage Prelude.Text
deleteClusterSnapshotMessage_snapshotIdentifier = Lens.lens (\DeleteClusterSnapshotMessage' {snapshotIdentifier} -> snapshotIdentifier) (\s@DeleteClusterSnapshotMessage' {} a -> s {snapshotIdentifier = a} :: DeleteClusterSnapshotMessage)

instance
  Prelude.Hashable
    DeleteClusterSnapshotMessage

instance Prelude.NFData DeleteClusterSnapshotMessage

instance Prelude.ToQuery DeleteClusterSnapshotMessage where
  toQuery DeleteClusterSnapshotMessage' {..} =
    Prelude.mconcat
      [ "SnapshotClusterIdentifier"
          Prelude.=: snapshotClusterIdentifier,
        "SnapshotIdentifier" Prelude.=: snapshotIdentifier
      ]
