{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ReplicationRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ReplicationRule
  ( ReplicationRule (..),

    -- * Smart constructor
    mkReplicationRule,

    -- * Lenses
    rrStatus,
    rrDestination,
    rrDeleteMarkerReplication,
    rrPriority,
    rrPrefix,
    rrExistingObjectReplication,
    rrId,
    rrFilter,
    rrSourceSelectionCriteria,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.DeleteMarkerReplication
import Network.AWS.S3.Types.Destination
import Network.AWS.S3.Types.ExistingObjectReplication
import Network.AWS.S3.Types.ReplicationRuleFilter
import Network.AWS.S3.Types.ReplicationRuleStatus
import Network.AWS.S3.Types.SourceSelectionCriteria

-- | Specifies which Amazon S3 objects to replicate and where to store the replicas.
--
-- /See:/ 'mkReplicationRule' smart constructor.
data ReplicationRule = ReplicationRule'
  { -- | Specifies whether the rule is enabled.
    status :: ReplicationRuleStatus,
    -- | A container for information about the replication destination and its configurations including enabling the S3 Replication Time Control (S3 RTC).
    destination :: Destination,
    deleteMarkerReplication :: Lude.Maybe DeleteMarkerReplication,
    -- | The priority associated with the rule. If you specify multiple rules in a replication configuration, Amazon S3 prioritizes the rules to prevent conflicts when filtering. If two or more rules identify the same object based on a specified filter, the rule with higher priority takes precedence. For example:
    --
    --
    --     * Same object quality prefix-based filter criteria if prefixes you specified in multiple rules overlap
    --
    --
    --     * Same object qualify tag-based filter criteria specified in multiple rules
    --
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication.html Replication> in the /Amazon Simple Storage Service Developer Guide/ .
    priority :: Lude.Maybe Lude.Int,
    -- | An object key name prefix that identifies the object or objects to which the rule applies. The maximum prefix length is 1,024 characters. To include all objects in a bucket, specify an empty string.
    prefix :: Lude.Maybe Lude.Text,
    -- |
    existingObjectReplication :: Lude.Maybe ExistingObjectReplication,
    -- | A unique identifier for the rule. The maximum value is 255 characters.
    id :: Lude.Maybe Lude.Text,
    filter :: Lude.Maybe ReplicationRuleFilter,
    -- | A container that describes additional filters for identifying the source objects that you want to replicate. You can choose to enable or disable the replication of these objects. Currently, Amazon S3 supports only the filter that you can specify for objects created with server-side encryption using a customer master key (CMK) stored in AWS Key Management Service (SSE-KMS).
    sourceSelectionCriteria :: Lude.Maybe SourceSelectionCriteria
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicationRule' with the minimum fields required to make a request.
--
-- * 'status' - Specifies whether the rule is enabled.
-- * 'destination' - A container for information about the replication destination and its configurations including enabling the S3 Replication Time Control (S3 RTC).
-- * 'deleteMarkerReplication' -
-- * 'priority' - The priority associated with the rule. If you specify multiple rules in a replication configuration, Amazon S3 prioritizes the rules to prevent conflicts when filtering. If two or more rules identify the same object based on a specified filter, the rule with higher priority takes precedence. For example:
--
--
--     * Same object quality prefix-based filter criteria if prefixes you specified in multiple rules overlap
--
--
--     * Same object qualify tag-based filter criteria specified in multiple rules
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication.html Replication> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'prefix' - An object key name prefix that identifies the object or objects to which the rule applies. The maximum prefix length is 1,024 characters. To include all objects in a bucket, specify an empty string.
-- * 'existingObjectReplication' -
-- * 'id' - A unique identifier for the rule. The maximum value is 255 characters.
-- * 'filter' -
-- * 'sourceSelectionCriteria' - A container that describes additional filters for identifying the source objects that you want to replicate. You can choose to enable or disable the replication of these objects. Currently, Amazon S3 supports only the filter that you can specify for objects created with server-side encryption using a customer master key (CMK) stored in AWS Key Management Service (SSE-KMS).
mkReplicationRule ::
  -- | 'status'
  ReplicationRuleStatus ->
  -- | 'destination'
  Destination ->
  ReplicationRule
mkReplicationRule pStatus_ pDestination_ =
  ReplicationRule'
    { status = pStatus_,
      destination = pDestination_,
      deleteMarkerReplication = Lude.Nothing,
      priority = Lude.Nothing,
      prefix = Lude.Nothing,
      existingObjectReplication = Lude.Nothing,
      id = Lude.Nothing,
      filter = Lude.Nothing,
      sourceSelectionCriteria = Lude.Nothing
    }

-- | Specifies whether the rule is enabled.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrStatus :: Lens.Lens' ReplicationRule ReplicationRuleStatus
rrStatus = Lens.lens (status :: ReplicationRule -> ReplicationRuleStatus) (\s a -> s {status = a} :: ReplicationRule)
{-# DEPRECATED rrStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A container for information about the replication destination and its configurations including enabling the S3 Replication Time Control (S3 RTC).
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDestination :: Lens.Lens' ReplicationRule Destination
rrDestination = Lens.lens (destination :: ReplicationRule -> Destination) (\s a -> s {destination = a} :: ReplicationRule)
{-# DEPRECATED rrDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'deleteMarkerReplication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDeleteMarkerReplication :: Lens.Lens' ReplicationRule (Lude.Maybe DeleteMarkerReplication)
rrDeleteMarkerReplication = Lens.lens (deleteMarkerReplication :: ReplicationRule -> Lude.Maybe DeleteMarkerReplication) (\s a -> s {deleteMarkerReplication = a} :: ReplicationRule)
{-# DEPRECATED rrDeleteMarkerReplication "Use generic-lens or generic-optics with 'deleteMarkerReplication' instead." #-}

-- | The priority associated with the rule. If you specify multiple rules in a replication configuration, Amazon S3 prioritizes the rules to prevent conflicts when filtering. If two or more rules identify the same object based on a specified filter, the rule with higher priority takes precedence. For example:
--
--
--     * Same object quality prefix-based filter criteria if prefixes you specified in multiple rules overlap
--
--
--     * Same object qualify tag-based filter criteria specified in multiple rules
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication.html Replication> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrPriority :: Lens.Lens' ReplicationRule (Lude.Maybe Lude.Int)
rrPriority = Lens.lens (priority :: ReplicationRule -> Lude.Maybe Lude.Int) (\s a -> s {priority = a} :: ReplicationRule)
{-# DEPRECATED rrPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | An object key name prefix that identifies the object or objects to which the rule applies. The maximum prefix length is 1,024 characters. To include all objects in a bucket, specify an empty string.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrPrefix :: Lens.Lens' ReplicationRule (Lude.Maybe Lude.Text)
rrPrefix = Lens.lens (prefix :: ReplicationRule -> Lude.Maybe Lude.Text) (\s a -> s {prefix = a} :: ReplicationRule)
{-# DEPRECATED rrPrefix "Use generic-lens or generic-optics with 'prefix' instead." #-}

-- |
--
-- /Note:/ Consider using 'existingObjectReplication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrExistingObjectReplication :: Lens.Lens' ReplicationRule (Lude.Maybe ExistingObjectReplication)
rrExistingObjectReplication = Lens.lens (existingObjectReplication :: ReplicationRule -> Lude.Maybe ExistingObjectReplication) (\s a -> s {existingObjectReplication = a} :: ReplicationRule)
{-# DEPRECATED rrExistingObjectReplication "Use generic-lens or generic-optics with 'existingObjectReplication' instead." #-}

-- | A unique identifier for the rule. The maximum value is 255 characters.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrId :: Lens.Lens' ReplicationRule (Lude.Maybe Lude.Text)
rrId = Lens.lens (id :: ReplicationRule -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ReplicationRule)
{-# DEPRECATED rrId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrFilter :: Lens.Lens' ReplicationRule (Lude.Maybe ReplicationRuleFilter)
rrFilter = Lens.lens (filter :: ReplicationRule -> Lude.Maybe ReplicationRuleFilter) (\s a -> s {filter = a} :: ReplicationRule)
{-# DEPRECATED rrFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | A container that describes additional filters for identifying the source objects that you want to replicate. You can choose to enable or disable the replication of these objects. Currently, Amazon S3 supports only the filter that you can specify for objects created with server-side encryption using a customer master key (CMK) stored in AWS Key Management Service (SSE-KMS).
--
-- /Note:/ Consider using 'sourceSelectionCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrSourceSelectionCriteria :: Lens.Lens' ReplicationRule (Lude.Maybe SourceSelectionCriteria)
rrSourceSelectionCriteria = Lens.lens (sourceSelectionCriteria :: ReplicationRule -> Lude.Maybe SourceSelectionCriteria) (\s a -> s {sourceSelectionCriteria = a} :: ReplicationRule)
{-# DEPRECATED rrSourceSelectionCriteria "Use generic-lens or generic-optics with 'sourceSelectionCriteria' instead." #-}

instance Lude.FromXML ReplicationRule where
  parseXML x =
    ReplicationRule'
      Lude.<$> (x Lude..@ "Status")
      Lude.<*> (x Lude..@ "Destination")
      Lude.<*> (x Lude..@? "DeleteMarkerReplication")
      Lude.<*> (x Lude..@? "Priority")
      Lude.<*> (x Lude..@? "Prefix")
      Lude.<*> (x Lude..@? "ExistingObjectReplication")
      Lude.<*> (x Lude..@? "ID")
      Lude.<*> (x Lude..@? "Filter")
      Lude.<*> (x Lude..@? "SourceSelectionCriteria")

instance Lude.ToXML ReplicationRule where
  toXML ReplicationRule' {..} =
    Lude.mconcat
      [ "Status" Lude.@= status,
        "Destination" Lude.@= destination,
        "DeleteMarkerReplication" Lude.@= deleteMarkerReplication,
        "Priority" Lude.@= priority,
        "Prefix" Lude.@= prefix,
        "ExistingObjectReplication" Lude.@= existingObjectReplication,
        "ID" Lude.@= id,
        "Filter" Lude.@= filter,
        "SourceSelectionCriteria" Lude.@= sourceSelectionCriteria
      ]
