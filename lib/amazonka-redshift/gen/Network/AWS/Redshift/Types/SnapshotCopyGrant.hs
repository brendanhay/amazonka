{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.SnapshotCopyGrant
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.SnapshotCopyGrant
  ( SnapshotCopyGrant (..),

    -- * Smart constructor
    mkSnapshotCopyGrant,

    -- * Lenses
    scgKmsKeyId,
    scgSnapshotCopyGrantName,
    scgTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.KmsKeyId as Types
import qualified Network.AWS.Redshift.Types.SnapshotCopyGrantName as Types
import qualified Network.AWS.Redshift.Types.Tag as Types

-- | The snapshot copy grant that grants Amazon Redshift permission to encrypt copied snapshots with the specified customer master key (CMK) from AWS KMS in the destination region.
--
-- For more information about managing snapshot copy grants, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html Amazon Redshift Database Encryption> in the /Amazon Redshift Cluster Management Guide/ .
--
-- /See:/ 'mkSnapshotCopyGrant' smart constructor.
data SnapshotCopyGrant = SnapshotCopyGrant'
  { -- | The unique identifier of the customer master key (CMK) in AWS KMS to which Amazon Redshift is granted permission.
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | The name of the snapshot copy grant.
    snapshotCopyGrantName :: Core.Maybe Types.SnapshotCopyGrantName,
    -- | A list of tag instances.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SnapshotCopyGrant' value with any optional fields omitted.
mkSnapshotCopyGrant ::
  SnapshotCopyGrant
mkSnapshotCopyGrant =
  SnapshotCopyGrant'
    { kmsKeyId = Core.Nothing,
      snapshotCopyGrantName = Core.Nothing,
      tags = Core.Nothing
    }

-- | The unique identifier of the customer master key (CMK) in AWS KMS to which Amazon Redshift is granted permission.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scgKmsKeyId :: Lens.Lens' SnapshotCopyGrant (Core.Maybe Types.KmsKeyId)
scgKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED scgKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The name of the snapshot copy grant.
--
-- /Note:/ Consider using 'snapshotCopyGrantName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scgSnapshotCopyGrantName :: Lens.Lens' SnapshotCopyGrant (Core.Maybe Types.SnapshotCopyGrantName)
scgSnapshotCopyGrantName = Lens.field @"snapshotCopyGrantName"
{-# DEPRECATED scgSnapshotCopyGrantName "Use generic-lens or generic-optics with 'snapshotCopyGrantName' instead." #-}

-- | A list of tag instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scgTags :: Lens.Lens' SnapshotCopyGrant (Core.Maybe [Types.Tag])
scgTags = Lens.field @"tags"
{-# DEPRECATED scgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromXML SnapshotCopyGrant where
  parseXML x =
    SnapshotCopyGrant'
      Core.<$> (x Core..@? "KmsKeyId")
      Core.<*> (x Core..@? "SnapshotCopyGrantName")
      Core.<*> (x Core..@? "Tags" Core..<@> Core.parseXMLList "Tag")
