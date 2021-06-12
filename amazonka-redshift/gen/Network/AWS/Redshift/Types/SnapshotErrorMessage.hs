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
-- Module      : Network.AWS.Redshift.Types.SnapshotErrorMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.SnapshotErrorMessage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal

-- | Describes the errors returned by a snapshot.
--
-- /See:/ 'newSnapshotErrorMessage' smart constructor.
data SnapshotErrorMessage = SnapshotErrorMessage'
  { -- | A unique identifier for the snapshot returning the error.
    snapshotIdentifier :: Core.Maybe Core.Text,
    -- | The failure code for the error.
    failureCode :: Core.Maybe Core.Text,
    -- | A unique identifier for the cluster.
    snapshotClusterIdentifier :: Core.Maybe Core.Text,
    -- | The text message describing the error.
    failureReason :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SnapshotErrorMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotIdentifier', 'snapshotErrorMessage_snapshotIdentifier' - A unique identifier for the snapshot returning the error.
--
-- 'failureCode', 'snapshotErrorMessage_failureCode' - The failure code for the error.
--
-- 'snapshotClusterIdentifier', 'snapshotErrorMessage_snapshotClusterIdentifier' - A unique identifier for the cluster.
--
-- 'failureReason', 'snapshotErrorMessage_failureReason' - The text message describing the error.
newSnapshotErrorMessage ::
  SnapshotErrorMessage
newSnapshotErrorMessage =
  SnapshotErrorMessage'
    { snapshotIdentifier =
        Core.Nothing,
      failureCode = Core.Nothing,
      snapshotClusterIdentifier = Core.Nothing,
      failureReason = Core.Nothing
    }

-- | A unique identifier for the snapshot returning the error.
snapshotErrorMessage_snapshotIdentifier :: Lens.Lens' SnapshotErrorMessage (Core.Maybe Core.Text)
snapshotErrorMessage_snapshotIdentifier = Lens.lens (\SnapshotErrorMessage' {snapshotIdentifier} -> snapshotIdentifier) (\s@SnapshotErrorMessage' {} a -> s {snapshotIdentifier = a} :: SnapshotErrorMessage)

-- | The failure code for the error.
snapshotErrorMessage_failureCode :: Lens.Lens' SnapshotErrorMessage (Core.Maybe Core.Text)
snapshotErrorMessage_failureCode = Lens.lens (\SnapshotErrorMessage' {failureCode} -> failureCode) (\s@SnapshotErrorMessage' {} a -> s {failureCode = a} :: SnapshotErrorMessage)

-- | A unique identifier for the cluster.
snapshotErrorMessage_snapshotClusterIdentifier :: Lens.Lens' SnapshotErrorMessage (Core.Maybe Core.Text)
snapshotErrorMessage_snapshotClusterIdentifier = Lens.lens (\SnapshotErrorMessage' {snapshotClusterIdentifier} -> snapshotClusterIdentifier) (\s@SnapshotErrorMessage' {} a -> s {snapshotClusterIdentifier = a} :: SnapshotErrorMessage)

-- | The text message describing the error.
snapshotErrorMessage_failureReason :: Lens.Lens' SnapshotErrorMessage (Core.Maybe Core.Text)
snapshotErrorMessage_failureReason = Lens.lens (\SnapshotErrorMessage' {failureReason} -> failureReason) (\s@SnapshotErrorMessage' {} a -> s {failureReason = a} :: SnapshotErrorMessage)

instance Core.FromXML SnapshotErrorMessage where
  parseXML x =
    SnapshotErrorMessage'
      Core.<$> (x Core..@? "SnapshotIdentifier")
      Core.<*> (x Core..@? "FailureCode")
      Core.<*> (x Core..@? "SnapshotClusterIdentifier")
      Core.<*> (x Core..@? "FailureReason")

instance Core.Hashable SnapshotErrorMessage

instance Core.NFData SnapshotErrorMessage
