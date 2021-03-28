{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ReplicationRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.ReplicationRule
  ( ReplicationRule (..)
  -- * Smart constructor
  , mkReplicationRule
  -- * Lenses
  , rrStatus
  , rrDestination
  , rrDeleteMarkerReplication
  , rrExistingObjectReplication
  , rrFilter
  , rrID
  , rrPrefix
  , rrPriority
  , rrSourceSelectionCriteria
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.DeleteMarkerReplication as Types
import qualified Network.AWS.S3.Types.Destination as Types
import qualified Network.AWS.S3.Types.ExistingObjectReplication as Types
import qualified Network.AWS.S3.Types.ID as Types
import qualified Network.AWS.S3.Types.Prefix as Types
import qualified Network.AWS.S3.Types.ReplicationRuleFilter as Types
import qualified Network.AWS.S3.Types.ReplicationRuleStatus as Types
import qualified Network.AWS.S3.Types.SourceSelectionCriteria as Types

-- | Specifies which Amazon S3 objects to replicate and where to store the replicas.
--
-- /See:/ 'mkReplicationRule' smart constructor.
data ReplicationRule = ReplicationRule'
  { status :: Types.ReplicationRuleStatus
    -- ^ Specifies whether the rule is enabled.
  , destination :: Types.Destination
    -- ^ A container for information about the replication destination and its configurations including enabling the S3 Replication Time Control (S3 RTC).
  , deleteMarkerReplication :: Core.Maybe Types.DeleteMarkerReplication
  , existingObjectReplication :: Core.Maybe Types.ExistingObjectReplication
    -- ^ 
  , filter :: Core.Maybe Types.ReplicationRuleFilter
  , id :: Core.Maybe Types.ID
    -- ^ A unique identifier for the rule. The maximum value is 255 characters.
  , prefix :: Core.Maybe Types.Prefix
    -- ^ An object key name prefix that identifies the object or objects to which the rule applies. The maximum prefix length is 1,024 characters. To include all objects in a bucket, specify an empty string. 
  , priority :: Core.Maybe Core.Int
    -- ^ The priority associated with the rule. If you specify multiple rules in a replication configuration, Amazon S3 prioritizes the rules to prevent conflicts when filtering. If two or more rules identify the same object based on a specified filter, the rule with higher priority takes precedence. For example:
--
--
--     * Same object quality prefix-based filter criteria if prefixes you specified in multiple rules overlap 
--
--
--     * Same object qualify tag-based filter criteria specified in multiple rules
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication.html Replication> in the /Amazon Simple Storage Service Developer Guide/ .
  , sourceSelectionCriteria :: Core.Maybe Types.SourceSelectionCriteria
    -- ^ A container that describes additional filters for identifying the source objects that you want to replicate. You can choose to enable or disable the replication of these objects. Currently, Amazon S3 supports only the filter that you can specify for objects created with server-side encryption using a customer master key (CMK) stored in AWS Key Management Service (SSE-KMS).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplicationRule' value with any optional fields omitted.
mkReplicationRule
    :: Types.ReplicationRuleStatus -- ^ 'status'
    -> Types.Destination -- ^ 'destination'
    -> ReplicationRule
mkReplicationRule status destination
  = ReplicationRule'{status, destination,
                     deleteMarkerReplication = Core.Nothing,
                     existingObjectReplication = Core.Nothing, filter = Core.Nothing,
                     id = Core.Nothing, prefix = Core.Nothing, priority = Core.Nothing,
                     sourceSelectionCriteria = Core.Nothing}

-- | Specifies whether the rule is enabled.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrStatus :: Lens.Lens' ReplicationRule Types.ReplicationRuleStatus
rrStatus = Lens.field @"status"
{-# INLINEABLE rrStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | A container for information about the replication destination and its configurations including enabling the S3 Replication Time Control (S3 RTC).
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDestination :: Lens.Lens' ReplicationRule Types.Destination
rrDestination = Lens.field @"destination"
{-# INLINEABLE rrDestination #-}
{-# DEPRECATED destination "Use generic-lens or generic-optics with 'destination' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'deleteMarkerReplication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrDeleteMarkerReplication :: Lens.Lens' ReplicationRule (Core.Maybe Types.DeleteMarkerReplication)
rrDeleteMarkerReplication = Lens.field @"deleteMarkerReplication"
{-# INLINEABLE rrDeleteMarkerReplication #-}
{-# DEPRECATED deleteMarkerReplication "Use generic-lens or generic-optics with 'deleteMarkerReplication' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'existingObjectReplication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrExistingObjectReplication :: Lens.Lens' ReplicationRule (Core.Maybe Types.ExistingObjectReplication)
rrExistingObjectReplication = Lens.field @"existingObjectReplication"
{-# INLINEABLE rrExistingObjectReplication #-}
{-# DEPRECATED existingObjectReplication "Use generic-lens or generic-optics with 'existingObjectReplication' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrFilter :: Lens.Lens' ReplicationRule (Core.Maybe Types.ReplicationRuleFilter)
rrFilter = Lens.field @"filter"
{-# INLINEABLE rrFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | A unique identifier for the rule. The maximum value is 255 characters.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrID :: Lens.Lens' ReplicationRule (Core.Maybe Types.ID)
rrID = Lens.field @"id"
{-# INLINEABLE rrID #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | An object key name prefix that identifies the object or objects to which the rule applies. The maximum prefix length is 1,024 characters. To include all objects in a bucket, specify an empty string. 
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrPrefix :: Lens.Lens' ReplicationRule (Core.Maybe Types.Prefix)
rrPrefix = Lens.field @"prefix"
{-# INLINEABLE rrPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

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
rrPriority :: Lens.Lens' ReplicationRule (Core.Maybe Core.Int)
rrPriority = Lens.field @"priority"
{-# INLINEABLE rrPriority #-}
{-# DEPRECATED priority "Use generic-lens or generic-optics with 'priority' instead"  #-}

-- | A container that describes additional filters for identifying the source objects that you want to replicate. You can choose to enable or disable the replication of these objects. Currently, Amazon S3 supports only the filter that you can specify for objects created with server-side encryption using a customer master key (CMK) stored in AWS Key Management Service (SSE-KMS).
--
-- /Note:/ Consider using 'sourceSelectionCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrSourceSelectionCriteria :: Lens.Lens' ReplicationRule (Core.Maybe Types.SourceSelectionCriteria)
rrSourceSelectionCriteria = Lens.field @"sourceSelectionCriteria"
{-# INLINEABLE rrSourceSelectionCriteria #-}
{-# DEPRECATED sourceSelectionCriteria "Use generic-lens or generic-optics with 'sourceSelectionCriteria' instead"  #-}

instance Core.ToXML ReplicationRule where
        toXML ReplicationRule{..}
          = Core.toXMLElement "Status" status Core.<>
              Core.toXMLElement "Destination" destination
              Core.<>
              Core.maybe Core.mempty
                (Core.toXMLElement "DeleteMarkerReplication")
                deleteMarkerReplication
              Core.<>
              Core.maybe Core.mempty
                (Core.toXMLElement "ExistingObjectReplication")
                existingObjectReplication
              Core.<> Core.maybe Core.mempty (Core.toXMLElement "Filter") filter
              Core.<> Core.maybe Core.mempty (Core.toXMLElement "ID") id
              Core.<> Core.maybe Core.mempty (Core.toXMLElement "Prefix") prefix
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Priority") priority
              Core.<>
              Core.maybe Core.mempty
                (Core.toXMLElement "SourceSelectionCriteria")
                sourceSelectionCriteria

instance Core.FromXML ReplicationRule where
        parseXML x
          = ReplicationRule' Core.<$>
              (x Core..@ "Status") Core.<*> x Core..@ "Destination" Core.<*>
                x Core..@? "DeleteMarkerReplication"
                Core.<*> x Core..@? "ExistingObjectReplication"
                Core.<*> x Core..@? "Filter"
                Core.<*> x Core..@? "ID"
                Core.<*> x Core..@? "Prefix"
                Core.<*> x Core..@? "Priority"
                Core.<*> x Core..@? "SourceSelectionCriteria"
