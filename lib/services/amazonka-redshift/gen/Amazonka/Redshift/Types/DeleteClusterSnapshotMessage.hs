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
-- Module      : Amazonka.Redshift.Types.DeleteClusterSnapshotMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.DeleteClusterSnapshotMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  hashWithSalt _salt DeleteClusterSnapshotMessage' {..} =
    _salt
      `Prelude.hashWithSalt` snapshotClusterIdentifier
      `Prelude.hashWithSalt` snapshotIdentifier

instance Prelude.NFData DeleteClusterSnapshotMessage where
  rnf DeleteClusterSnapshotMessage' {..} =
    Prelude.rnf snapshotClusterIdentifier `Prelude.seq`
      Prelude.rnf snapshotIdentifier

instance Data.ToQuery DeleteClusterSnapshotMessage where
  toQuery DeleteClusterSnapshotMessage' {..} =
    Prelude.mconcat
      [ "SnapshotClusterIdentifier"
          Data.=: snapshotClusterIdentifier,
        "SnapshotIdentifier" Data.=: snapshotIdentifier
      ]
