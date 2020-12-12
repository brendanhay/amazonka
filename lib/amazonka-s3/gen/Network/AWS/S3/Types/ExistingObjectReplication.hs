{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ExistingObjectReplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ExistingObjectReplication
  ( ExistingObjectReplication (..),

    -- * Smart constructor
    mkExistingObjectReplication,

    -- * Lenses
    eorStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ExistingObjectReplicationStatus

-- | Optional configuration to replicate existing source bucket objects. For more information, see < https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-what-is-isnot-replicated.html#existing-object-replication Replicating Existing Objects> in the /Amazon S3 Developer Guide/ .
--
-- /See:/ 'mkExistingObjectReplication' smart constructor.
newtype ExistingObjectReplication = ExistingObjectReplication'
  { status ::
      ExistingObjectReplicationStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExistingObjectReplication' with the minimum fields required to make a request.
--
-- * 'status' -
mkExistingObjectReplication ::
  -- | 'status'
  ExistingObjectReplicationStatus ->
  ExistingObjectReplication
mkExistingObjectReplication pStatus_ =
  ExistingObjectReplication' {status = pStatus_}

-- |
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eorStatus :: Lens.Lens' ExistingObjectReplication ExistingObjectReplicationStatus
eorStatus = Lens.lens (status :: ExistingObjectReplication -> ExistingObjectReplicationStatus) (\s a -> s {status = a} :: ExistingObjectReplication)
{-# DEPRECATED eorStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromXML ExistingObjectReplication where
  parseXML x =
    ExistingObjectReplication' Lude.<$> (x Lude..@ "Status")

instance Lude.ToXML ExistingObjectReplication where
  toXML ExistingObjectReplication' {..} =
    Lude.mconcat ["Status" Lude.@= status]
