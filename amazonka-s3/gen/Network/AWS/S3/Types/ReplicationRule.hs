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
-- Module      : Network.AWS.S3.Types.ReplicationRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ReplicationRule where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.DeleteMarkerReplication
import Network.AWS.S3.Types.Destination
import Network.AWS.S3.Types.ExistingObjectReplication
import Network.AWS.S3.Types.ReplicationRuleFilter
import Network.AWS.S3.Types.ReplicationRuleStatus
import Network.AWS.S3.Types.SourceSelectionCriteria

-- | Specifies which Amazon S3 objects to replicate and where to store the
-- replicas.
--
-- /See:/ 'newReplicationRule' smart constructor.
data ReplicationRule = ReplicationRule'
  { -- | An object key name prefix that identifies the object or objects to which
    -- the rule applies. The maximum prefix length is 1,024 characters. To
    -- include all objects in a bucket, specify an empty string.
    --
    -- Replacement must be made for object keys containing special characters
    -- (such as carriage returns) when using XML requests. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
    prefix :: Core.Maybe Core.Text,
    -- | A unique identifier for the rule. The maximum value is 255 characters.
    id :: Core.Maybe Core.Text,
    existingObjectReplication :: Core.Maybe ExistingObjectReplication,
    -- | The priority indicates which rule has precedence whenever two or more
    -- replication rules conflict. Amazon S3 will attempt to replicate objects
    -- according to all replication rules. However, if there are two or more
    -- rules with the same destination bucket, then objects will be replicated
    -- according to the rule with the highest priority. The higher the number,
    -- the higher the priority.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication.html Replication>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    priority :: Core.Maybe Core.Int,
    deleteMarkerReplication :: Core.Maybe DeleteMarkerReplication,
    -- | A container that describes additional filters for identifying the source
    -- objects that you want to replicate. You can choose to enable or disable
    -- the replication of these objects. Currently, Amazon S3 supports only the
    -- filter that you can specify for objects created with server-side
    -- encryption using a customer master key (CMK) stored in AWS Key
    -- Management Service (SSE-KMS).
    sourceSelectionCriteria :: Core.Maybe SourceSelectionCriteria,
    filter' :: Core.Maybe ReplicationRuleFilter,
    -- | Specifies whether the rule is enabled.
    status :: ReplicationRuleStatus,
    -- | A container for information about the replication destination and its
    -- configurations including enabling the S3 Replication Time Control (S3
    -- RTC).
    destination :: Destination
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReplicationRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 'replicationRule_prefix' - An object key name prefix that identifies the object or objects to which
-- the rule applies. The maximum prefix length is 1,024 characters. To
-- include all objects in a bucket, specify an empty string.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
--
-- 'id', 'replicationRule_id' - A unique identifier for the rule. The maximum value is 255 characters.
--
-- 'existingObjectReplication', 'replicationRule_existingObjectReplication' -
--
-- 'priority', 'replicationRule_priority' - The priority indicates which rule has precedence whenever two or more
-- replication rules conflict. Amazon S3 will attempt to replicate objects
-- according to all replication rules. However, if there are two or more
-- rules with the same destination bucket, then objects will be replicated
-- according to the rule with the highest priority. The higher the number,
-- the higher the priority.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication.html Replication>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- 'deleteMarkerReplication', 'replicationRule_deleteMarkerReplication' - Undocumented member.
--
-- 'sourceSelectionCriteria', 'replicationRule_sourceSelectionCriteria' - A container that describes additional filters for identifying the source
-- objects that you want to replicate. You can choose to enable or disable
-- the replication of these objects. Currently, Amazon S3 supports only the
-- filter that you can specify for objects created with server-side
-- encryption using a customer master key (CMK) stored in AWS Key
-- Management Service (SSE-KMS).
--
-- 'filter'', 'replicationRule_filter' - Undocumented member.
--
-- 'status', 'replicationRule_status' - Specifies whether the rule is enabled.
--
-- 'destination', 'replicationRule_destination' - A container for information about the replication destination and its
-- configurations including enabling the S3 Replication Time Control (S3
-- RTC).
newReplicationRule ::
  -- | 'status'
  ReplicationRuleStatus ->
  -- | 'destination'
  Destination ->
  ReplicationRule
newReplicationRule pStatus_ pDestination_ =
  ReplicationRule'
    { prefix = Core.Nothing,
      id = Core.Nothing,
      existingObjectReplication = Core.Nothing,
      priority = Core.Nothing,
      deleteMarkerReplication = Core.Nothing,
      sourceSelectionCriteria = Core.Nothing,
      filter' = Core.Nothing,
      status = pStatus_,
      destination = pDestination_
    }

-- | An object key name prefix that identifies the object or objects to which
-- the rule applies. The maximum prefix length is 1,024 characters. To
-- include all objects in a bucket, specify an empty string.
--
-- Replacement must be made for object keys containing special characters
-- (such as carriage returns) when using XML requests. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-keys.html#object-key-xml-related-constraints XML related object key constraints>.
replicationRule_prefix :: Lens.Lens' ReplicationRule (Core.Maybe Core.Text)
replicationRule_prefix = Lens.lens (\ReplicationRule' {prefix} -> prefix) (\s@ReplicationRule' {} a -> s {prefix = a} :: ReplicationRule)

-- | A unique identifier for the rule. The maximum value is 255 characters.
replicationRule_id :: Lens.Lens' ReplicationRule (Core.Maybe Core.Text)
replicationRule_id = Lens.lens (\ReplicationRule' {id} -> id) (\s@ReplicationRule' {} a -> s {id = a} :: ReplicationRule)

-- |
replicationRule_existingObjectReplication :: Lens.Lens' ReplicationRule (Core.Maybe ExistingObjectReplication)
replicationRule_existingObjectReplication = Lens.lens (\ReplicationRule' {existingObjectReplication} -> existingObjectReplication) (\s@ReplicationRule' {} a -> s {existingObjectReplication = a} :: ReplicationRule)

-- | The priority indicates which rule has precedence whenever two or more
-- replication rules conflict. Amazon S3 will attempt to replicate objects
-- according to all replication rules. However, if there are two or more
-- rules with the same destination bucket, then objects will be replicated
-- according to the rule with the highest priority. The higher the number,
-- the higher the priority.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication.html Replication>
-- in the /Amazon Simple Storage Service Developer Guide/.
replicationRule_priority :: Lens.Lens' ReplicationRule (Core.Maybe Core.Int)
replicationRule_priority = Lens.lens (\ReplicationRule' {priority} -> priority) (\s@ReplicationRule' {} a -> s {priority = a} :: ReplicationRule)

-- | Undocumented member.
replicationRule_deleteMarkerReplication :: Lens.Lens' ReplicationRule (Core.Maybe DeleteMarkerReplication)
replicationRule_deleteMarkerReplication = Lens.lens (\ReplicationRule' {deleteMarkerReplication} -> deleteMarkerReplication) (\s@ReplicationRule' {} a -> s {deleteMarkerReplication = a} :: ReplicationRule)

-- | A container that describes additional filters for identifying the source
-- objects that you want to replicate. You can choose to enable or disable
-- the replication of these objects. Currently, Amazon S3 supports only the
-- filter that you can specify for objects created with server-side
-- encryption using a customer master key (CMK) stored in AWS Key
-- Management Service (SSE-KMS).
replicationRule_sourceSelectionCriteria :: Lens.Lens' ReplicationRule (Core.Maybe SourceSelectionCriteria)
replicationRule_sourceSelectionCriteria = Lens.lens (\ReplicationRule' {sourceSelectionCriteria} -> sourceSelectionCriteria) (\s@ReplicationRule' {} a -> s {sourceSelectionCriteria = a} :: ReplicationRule)

-- | Undocumented member.
replicationRule_filter :: Lens.Lens' ReplicationRule (Core.Maybe ReplicationRuleFilter)
replicationRule_filter = Lens.lens (\ReplicationRule' {filter'} -> filter') (\s@ReplicationRule' {} a -> s {filter' = a} :: ReplicationRule)

-- | Specifies whether the rule is enabled.
replicationRule_status :: Lens.Lens' ReplicationRule ReplicationRuleStatus
replicationRule_status = Lens.lens (\ReplicationRule' {status} -> status) (\s@ReplicationRule' {} a -> s {status = a} :: ReplicationRule)

-- | A container for information about the replication destination and its
-- configurations including enabling the S3 Replication Time Control (S3
-- RTC).
replicationRule_destination :: Lens.Lens' ReplicationRule Destination
replicationRule_destination = Lens.lens (\ReplicationRule' {destination} -> destination) (\s@ReplicationRule' {} a -> s {destination = a} :: ReplicationRule)

instance Core.FromXML ReplicationRule where
  parseXML x =
    ReplicationRule'
      Core.<$> (x Core..@? "Prefix")
      Core.<*> (x Core..@? "ID")
      Core.<*> (x Core..@? "ExistingObjectReplication")
      Core.<*> (x Core..@? "Priority")
      Core.<*> (x Core..@? "DeleteMarkerReplication")
      Core.<*> (x Core..@? "SourceSelectionCriteria")
      Core.<*> (x Core..@? "Filter")
      Core.<*> (x Core..@ "Status")
      Core.<*> (x Core..@ "Destination")

instance Core.Hashable ReplicationRule

instance Core.NFData ReplicationRule

instance Core.ToXML ReplicationRule where
  toXML ReplicationRule' {..} =
    Core.mconcat
      [ "Prefix" Core.@= prefix,
        "ID" Core.@= id,
        "ExistingObjectReplication"
          Core.@= existingObjectReplication,
        "Priority" Core.@= priority,
        "DeleteMarkerReplication"
          Core.@= deleteMarkerReplication,
        "SourceSelectionCriteria"
          Core.@= sourceSelectionCriteria,
        "Filter" Core.@= filter',
        "Status" Core.@= status,
        "Destination" Core.@= destination
      ]
