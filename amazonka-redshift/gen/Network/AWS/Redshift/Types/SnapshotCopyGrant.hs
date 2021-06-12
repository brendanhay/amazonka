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
-- Module      : Network.AWS.Redshift.Types.SnapshotCopyGrant
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.SnapshotCopyGrant where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Tag

-- | The snapshot copy grant that grants Amazon Redshift permission to
-- encrypt copied snapshots with the specified customer master key (CMK)
-- from AWS KMS in the destination region.
--
-- For more information about managing snapshot copy grants, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html Amazon Redshift Database Encryption>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- /See:/ 'newSnapshotCopyGrant' smart constructor.
data SnapshotCopyGrant = SnapshotCopyGrant'
  { -- | The name of the snapshot copy grant.
    snapshotCopyGrantName :: Core.Maybe Core.Text,
    -- | The unique identifier of the customer master key (CMK) in AWS KMS to
    -- which Amazon Redshift is granted permission.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | A list of tag instances.
    tags :: Core.Maybe [Tag]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SnapshotCopyGrant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snapshotCopyGrantName', 'snapshotCopyGrant_snapshotCopyGrantName' - The name of the snapshot copy grant.
--
-- 'kmsKeyId', 'snapshotCopyGrant_kmsKeyId' - The unique identifier of the customer master key (CMK) in AWS KMS to
-- which Amazon Redshift is granted permission.
--
-- 'tags', 'snapshotCopyGrant_tags' - A list of tag instances.
newSnapshotCopyGrant ::
  SnapshotCopyGrant
newSnapshotCopyGrant =
  SnapshotCopyGrant'
    { snapshotCopyGrantName =
        Core.Nothing,
      kmsKeyId = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the snapshot copy grant.
snapshotCopyGrant_snapshotCopyGrantName :: Lens.Lens' SnapshotCopyGrant (Core.Maybe Core.Text)
snapshotCopyGrant_snapshotCopyGrantName = Lens.lens (\SnapshotCopyGrant' {snapshotCopyGrantName} -> snapshotCopyGrantName) (\s@SnapshotCopyGrant' {} a -> s {snapshotCopyGrantName = a} :: SnapshotCopyGrant)

-- | The unique identifier of the customer master key (CMK) in AWS KMS to
-- which Amazon Redshift is granted permission.
snapshotCopyGrant_kmsKeyId :: Lens.Lens' SnapshotCopyGrant (Core.Maybe Core.Text)
snapshotCopyGrant_kmsKeyId = Lens.lens (\SnapshotCopyGrant' {kmsKeyId} -> kmsKeyId) (\s@SnapshotCopyGrant' {} a -> s {kmsKeyId = a} :: SnapshotCopyGrant)

-- | A list of tag instances.
snapshotCopyGrant_tags :: Lens.Lens' SnapshotCopyGrant (Core.Maybe [Tag])
snapshotCopyGrant_tags = Lens.lens (\SnapshotCopyGrant' {tags} -> tags) (\s@SnapshotCopyGrant' {} a -> s {tags = a} :: SnapshotCopyGrant) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML SnapshotCopyGrant where
  parseXML x =
    SnapshotCopyGrant'
      Core.<$> (x Core..@? "SnapshotCopyGrantName")
      Core.<*> (x Core..@? "KmsKeyId")
      Core.<*> ( x Core..@? "Tags" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Tag")
               )

instance Core.Hashable SnapshotCopyGrant

instance Core.NFData SnapshotCopyGrant
