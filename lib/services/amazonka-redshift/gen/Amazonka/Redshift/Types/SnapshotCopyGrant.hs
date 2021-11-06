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
-- Module      : Amazonka.Redshift.Types.SnapshotCopyGrant
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.SnapshotCopyGrant where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.Tag

-- | The snapshot copy grant that grants Amazon Redshift permission to
-- encrypt copied snapshots with the specified customer master key (CMK)
-- from Amazon Web Services KMS in the destination region.
--
-- For more information about managing snapshot copy grants, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html Amazon Redshift Database Encryption>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- /See:/ 'newSnapshotCopyGrant' smart constructor.
data SnapshotCopyGrant = SnapshotCopyGrant'
  { -- | The unique identifier of the customer master key (CMK) in Amazon Web
    -- Services KMS to which Amazon Redshift is granted permission.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the snapshot copy grant.
    snapshotCopyGrantName :: Prelude.Maybe Prelude.Text,
    -- | A list of tag instances.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnapshotCopyGrant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'snapshotCopyGrant_kmsKeyId' - The unique identifier of the customer master key (CMK) in Amazon Web
-- Services KMS to which Amazon Redshift is granted permission.
--
-- 'snapshotCopyGrantName', 'snapshotCopyGrant_snapshotCopyGrantName' - The name of the snapshot copy grant.
--
-- 'tags', 'snapshotCopyGrant_tags' - A list of tag instances.
newSnapshotCopyGrant ::
  SnapshotCopyGrant
newSnapshotCopyGrant =
  SnapshotCopyGrant'
    { kmsKeyId = Prelude.Nothing,
      snapshotCopyGrantName = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The unique identifier of the customer master key (CMK) in Amazon Web
-- Services KMS to which Amazon Redshift is granted permission.
snapshotCopyGrant_kmsKeyId :: Lens.Lens' SnapshotCopyGrant (Prelude.Maybe Prelude.Text)
snapshotCopyGrant_kmsKeyId = Lens.lens (\SnapshotCopyGrant' {kmsKeyId} -> kmsKeyId) (\s@SnapshotCopyGrant' {} a -> s {kmsKeyId = a} :: SnapshotCopyGrant)

-- | The name of the snapshot copy grant.
snapshotCopyGrant_snapshotCopyGrantName :: Lens.Lens' SnapshotCopyGrant (Prelude.Maybe Prelude.Text)
snapshotCopyGrant_snapshotCopyGrantName = Lens.lens (\SnapshotCopyGrant' {snapshotCopyGrantName} -> snapshotCopyGrantName) (\s@SnapshotCopyGrant' {} a -> s {snapshotCopyGrantName = a} :: SnapshotCopyGrant)

-- | A list of tag instances.
snapshotCopyGrant_tags :: Lens.Lens' SnapshotCopyGrant (Prelude.Maybe [Tag])
snapshotCopyGrant_tags = Lens.lens (\SnapshotCopyGrant' {tags} -> tags) (\s@SnapshotCopyGrant' {} a -> s {tags = a} :: SnapshotCopyGrant) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML SnapshotCopyGrant where
  parseXML x =
    SnapshotCopyGrant'
      Prelude.<$> (x Core..@? "KmsKeyId")
      Prelude.<*> (x Core..@? "SnapshotCopyGrantName")
      Prelude.<*> ( x Core..@? "Tags" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "Tag")
                  )

instance Prelude.Hashable SnapshotCopyGrant

instance Prelude.NFData SnapshotCopyGrant
