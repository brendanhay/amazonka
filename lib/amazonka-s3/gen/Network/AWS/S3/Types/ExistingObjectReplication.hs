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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.ExistingObjectReplicationStatus as Types

-- | Optional configuration to replicate existing source bucket objects. For more information, see < https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-what-is-isnot-replicated.html#existing-object-replication Replicating Existing Objects> in the /Amazon S3 Developer Guide/ .
--
-- /See:/ 'mkExistingObjectReplication' smart constructor.
newtype ExistingObjectReplication = ExistingObjectReplication'
  { -- |
    status :: Types.ExistingObjectReplicationStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ExistingObjectReplication' value with any optional fields omitted.
mkExistingObjectReplication ::
  -- | 'status'
  Types.ExistingObjectReplicationStatus ->
  ExistingObjectReplication
mkExistingObjectReplication status =
  ExistingObjectReplication' {status}

-- |
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eorStatus :: Lens.Lens' ExistingObjectReplication Types.ExistingObjectReplicationStatus
eorStatus = Lens.field @"status"
{-# DEPRECATED eorStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.ToXML ExistingObjectReplication where
  toXML ExistingObjectReplication {..} =
    Core.toXMLNode "Status" status

instance Core.FromXML ExistingObjectReplication where
  parseXML x =
    ExistingObjectReplication' Core.<$> (x Core..@ "Status")
