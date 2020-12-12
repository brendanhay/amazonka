{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.DeleteMarkerReplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.DeleteMarkerReplication
  ( DeleteMarkerReplication (..),

    -- * Smart constructor
    mkDeleteMarkerReplication,

    -- * Lenses
    dmrStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.DeleteMarkerReplicationStatus

-- | Specifies whether Amazon S3 replicates delete markers. If you specify a @Filter@ in your replication configuration, you must also include a @DeleteMarkerReplication@ element. If your @Filter@ includes a @Tag@ element, the @DeleteMarkerReplication@ @Status@ must be set to Disabled, because Amazon S3 does not support replicating delete markers for tag-based rules. For an example configuration, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-add-config.html#replication-config-min-rule-config Basic Rule Configuration> .
--
-- For more information about delete marker replication, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/delete-marker-replication.html Basic Rule Configuration> .
--
-- /See:/ 'mkDeleteMarkerReplication' smart constructor.
newtype DeleteMarkerReplication = DeleteMarkerReplication'
  { status ::
      Lude.Maybe DeleteMarkerReplicationStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMarkerReplication' with the minimum fields required to make a request.
--
-- * 'status' - Indicates whether to replicate delete markers.
mkDeleteMarkerReplication ::
  DeleteMarkerReplication
mkDeleteMarkerReplication =
  DeleteMarkerReplication' {status = Lude.Nothing}

-- | Indicates whether to replicate delete markers.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmrStatus :: Lens.Lens' DeleteMarkerReplication (Lude.Maybe DeleteMarkerReplicationStatus)
dmrStatus = Lens.lens (status :: DeleteMarkerReplication -> Lude.Maybe DeleteMarkerReplicationStatus) (\s a -> s {status = a} :: DeleteMarkerReplication)
{-# DEPRECATED dmrStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromXML DeleteMarkerReplication where
  parseXML x =
    DeleteMarkerReplication' Lude.<$> (x Lude..@? "Status")

instance Lude.ToXML DeleteMarkerReplication where
  toXML DeleteMarkerReplication' {..} =
    Lude.mconcat ["Status" Lude.@= status]
