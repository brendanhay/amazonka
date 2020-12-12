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
    scgKMSKeyId,
    scgSnapshotCopyGrantName,
    scgTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Tag

-- | The snapshot copy grant that grants Amazon Redshift permission to encrypt copied snapshots with the specified customer master key (CMK) from AWS KMS in the destination region.
--
-- For more information about managing snapshot copy grants, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html Amazon Redshift Database Encryption> in the /Amazon Redshift Cluster Management Guide/ .
--
-- /See:/ 'mkSnapshotCopyGrant' smart constructor.
data SnapshotCopyGrant = SnapshotCopyGrant'
  { kmsKeyId ::
      Lude.Maybe Lude.Text,
    snapshotCopyGrantName :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SnapshotCopyGrant' with the minimum fields required to make a request.
--
-- * 'kmsKeyId' - The unique identifier of the customer master key (CMK) in AWS KMS to which Amazon Redshift is granted permission.
-- * 'snapshotCopyGrantName' - The name of the snapshot copy grant.
-- * 'tags' - A list of tag instances.
mkSnapshotCopyGrant ::
  SnapshotCopyGrant
mkSnapshotCopyGrant =
  SnapshotCopyGrant'
    { kmsKeyId = Lude.Nothing,
      snapshotCopyGrantName = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The unique identifier of the customer master key (CMK) in AWS KMS to which Amazon Redshift is granted permission.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scgKMSKeyId :: Lens.Lens' SnapshotCopyGrant (Lude.Maybe Lude.Text)
scgKMSKeyId = Lens.lens (kmsKeyId :: SnapshotCopyGrant -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: SnapshotCopyGrant)
{-# DEPRECATED scgKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The name of the snapshot copy grant.
--
-- /Note:/ Consider using 'snapshotCopyGrantName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scgSnapshotCopyGrantName :: Lens.Lens' SnapshotCopyGrant (Lude.Maybe Lude.Text)
scgSnapshotCopyGrantName = Lens.lens (snapshotCopyGrantName :: SnapshotCopyGrant -> Lude.Maybe Lude.Text) (\s a -> s {snapshotCopyGrantName = a} :: SnapshotCopyGrant)
{-# DEPRECATED scgSnapshotCopyGrantName "Use generic-lens or generic-optics with 'snapshotCopyGrantName' instead." #-}

-- | A list of tag instances.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scgTags :: Lens.Lens' SnapshotCopyGrant (Lude.Maybe [Tag])
scgTags = Lens.lens (tags :: SnapshotCopyGrant -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: SnapshotCopyGrant)
{-# DEPRECATED scgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML SnapshotCopyGrant where
  parseXML x =
    SnapshotCopyGrant'
      Lude.<$> (x Lude..@? "KmsKeyId")
      Lude.<*> (x Lude..@? "SnapshotCopyGrantName")
      Lude.<*> ( x Lude..@? "Tags" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Tag")
               )
