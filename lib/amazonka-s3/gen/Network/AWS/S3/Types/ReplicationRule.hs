{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ReplicationRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ReplicationRule where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.DeleteMarkerReplication
import Network.AWS.S3.Types.Destination
import Network.AWS.S3.Types.ExistingObjectReplication
import Network.AWS.S3.Types.ReplicationRuleFilter
import Network.AWS.S3.Types.ReplicationRuleStatus
import Network.AWS.S3.Types.SourceSelectionCriteria

-- | Specifies which Amazon S3 objects to replicate and where to store the replicas.
--
--
--
-- /See:/ 'replicationRule' smart constructor.
data ReplicationRule = ReplicationRule'
  { _rrDeleteMarkerReplication ::
      !(Maybe DeleteMarkerReplication),
    _rrPriority :: !(Maybe Int),
    _rrPrefix :: !(Maybe Text),
    _rrExistingObjectReplication ::
      !(Maybe ExistingObjectReplication),
    _rrId :: !(Maybe Text),
    _rrFilter :: !(Maybe ReplicationRuleFilter),
    _rrSourceSelectionCriteria ::
      !(Maybe SourceSelectionCriteria),
    _rrStatus :: !ReplicationRuleStatus,
    _rrDestination :: !Destination
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicationRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrDeleteMarkerReplication' - Undocumented member.
--
-- * 'rrPriority' - The priority associated with the rule. If you specify multiple rules in a replication configuration, Amazon S3 prioritizes the rules to prevent conflicts when filtering. If two or more rules identify the same object based on a specified filter, the rule with higher priority takes precedence. For example:     * Same object quality prefix-based filter criteria if prefixes you specified in multiple rules overlap      * Same object qualify tag-based filter criteria specified in multiple rules For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication.html Replication> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'rrPrefix' - An object key name prefix that identifies the object or objects to which the rule applies. The maximum prefix length is 1,024 characters. To include all objects in a bucket, specify an empty string.
--
-- * 'rrExistingObjectReplication' -
--
-- * 'rrId' - A unique identifier for the rule. The maximum value is 255 characters.
--
-- * 'rrFilter' - Undocumented member.
--
-- * 'rrSourceSelectionCriteria' - A container that describes additional filters for identifying the source objects that you want to replicate. You can choose to enable or disable the replication of these objects. Currently, Amazon S3 supports only the filter that you can specify for objects created with server-side encryption using a customer master key (CMK) stored in AWS Key Management Service (SSE-KMS).
--
-- * 'rrStatus' - Specifies whether the rule is enabled.
--
-- * 'rrDestination' - A container for information about the replication destination and its configurations including enabling the S3 Replication Time Control (S3 RTC).
replicationRule ::
  -- | 'rrStatus'
  ReplicationRuleStatus ->
  -- | 'rrDestination'
  Destination ->
  ReplicationRule
replicationRule pStatus_ pDestination_ =
  ReplicationRule'
    { _rrDeleteMarkerReplication = Nothing,
      _rrPriority = Nothing,
      _rrPrefix = Nothing,
      _rrExistingObjectReplication = Nothing,
      _rrId = Nothing,
      _rrFilter = Nothing,
      _rrSourceSelectionCriteria = Nothing,
      _rrStatus = pStatus_,
      _rrDestination = pDestination_
    }

-- | Undocumented member.
rrDeleteMarkerReplication :: Lens' ReplicationRule (Maybe DeleteMarkerReplication)
rrDeleteMarkerReplication = lens _rrDeleteMarkerReplication (\s a -> s {_rrDeleteMarkerReplication = a})

-- | The priority associated with the rule. If you specify multiple rules in a replication configuration, Amazon S3 prioritizes the rules to prevent conflicts when filtering. If two or more rules identify the same object based on a specified filter, the rule with higher priority takes precedence. For example:     * Same object quality prefix-based filter criteria if prefixes you specified in multiple rules overlap      * Same object qualify tag-based filter criteria specified in multiple rules For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication.html Replication> in the /Amazon Simple Storage Service Developer Guide/ .
rrPriority :: Lens' ReplicationRule (Maybe Int)
rrPriority = lens _rrPriority (\s a -> s {_rrPriority = a})

-- | An object key name prefix that identifies the object or objects to which the rule applies. The maximum prefix length is 1,024 characters. To include all objects in a bucket, specify an empty string.
rrPrefix :: Lens' ReplicationRule (Maybe Text)
rrPrefix = lens _rrPrefix (\s a -> s {_rrPrefix = a})

-- |
rrExistingObjectReplication :: Lens' ReplicationRule (Maybe ExistingObjectReplication)
rrExistingObjectReplication = lens _rrExistingObjectReplication (\s a -> s {_rrExistingObjectReplication = a})

-- | A unique identifier for the rule. The maximum value is 255 characters.
rrId :: Lens' ReplicationRule (Maybe Text)
rrId = lens _rrId (\s a -> s {_rrId = a})

-- | Undocumented member.
rrFilter :: Lens' ReplicationRule (Maybe ReplicationRuleFilter)
rrFilter = lens _rrFilter (\s a -> s {_rrFilter = a})

-- | A container that describes additional filters for identifying the source objects that you want to replicate. You can choose to enable or disable the replication of these objects. Currently, Amazon S3 supports only the filter that you can specify for objects created with server-side encryption using a customer master key (CMK) stored in AWS Key Management Service (SSE-KMS).
rrSourceSelectionCriteria :: Lens' ReplicationRule (Maybe SourceSelectionCriteria)
rrSourceSelectionCriteria = lens _rrSourceSelectionCriteria (\s a -> s {_rrSourceSelectionCriteria = a})

-- | Specifies whether the rule is enabled.
rrStatus :: Lens' ReplicationRule ReplicationRuleStatus
rrStatus = lens _rrStatus (\s a -> s {_rrStatus = a})

-- | A container for information about the replication destination and its configurations including enabling the S3 Replication Time Control (S3 RTC).
rrDestination :: Lens' ReplicationRule Destination
rrDestination = lens _rrDestination (\s a -> s {_rrDestination = a})

instance FromXML ReplicationRule where
  parseXML x =
    ReplicationRule'
      <$> (x .@? "DeleteMarkerReplication")
      <*> (x .@? "Priority")
      <*> (x .@? "Prefix")
      <*> (x .@? "ExistingObjectReplication")
      <*> (x .@? "ID")
      <*> (x .@? "Filter")
      <*> (x .@? "SourceSelectionCriteria")
      <*> (x .@ "Status")
      <*> (x .@ "Destination")

instance Hashable ReplicationRule

instance NFData ReplicationRule

instance ToXML ReplicationRule where
  toXML ReplicationRule' {..} =
    mconcat
      [ "DeleteMarkerReplication" @= _rrDeleteMarkerReplication,
        "Priority" @= _rrPriority,
        "Prefix" @= _rrPrefix,
        "ExistingObjectReplication" @= _rrExistingObjectReplication,
        "ID" @= _rrId,
        "Filter" @= _rrFilter,
        "SourceSelectionCriteria" @= _rrSourceSelectionCriteria,
        "Status" @= _rrStatus,
        "Destination" @= _rrDestination
      ]
