{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationSubnetGroup where

import Network.AWS.DMS.Types.Subnet
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a subnet group in response to a request by the @DescribeReplicationSubnetGroups@ operation.
--
--
--
-- /See:/ 'replicationSubnetGroup' smart constructor.
data ReplicationSubnetGroup = ReplicationSubnetGroup'
  { _rsgVPCId ::
      !(Maybe Text),
    _rsgSubnets :: !(Maybe [Subnet]),
    _rsgReplicationSubnetGroupIdentifier ::
      !(Maybe Text),
    _rsgSubnetGroupStatus :: !(Maybe Text),
    _rsgReplicationSubnetGroupDescription ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicationSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsgVPCId' - The ID of the VPC.
--
-- * 'rsgSubnets' - The subnets that are in the subnet group.
--
-- * 'rsgReplicationSubnetGroupIdentifier' - The identifier of the replication instance subnet group.
--
-- * 'rsgSubnetGroupStatus' - The status of the subnet group.
--
-- * 'rsgReplicationSubnetGroupDescription' - A description for the replication subnet group.
replicationSubnetGroup ::
  ReplicationSubnetGroup
replicationSubnetGroup =
  ReplicationSubnetGroup'
    { _rsgVPCId = Nothing,
      _rsgSubnets = Nothing,
      _rsgReplicationSubnetGroupIdentifier = Nothing,
      _rsgSubnetGroupStatus = Nothing,
      _rsgReplicationSubnetGroupDescription = Nothing
    }

-- | The ID of the VPC.
rsgVPCId :: Lens' ReplicationSubnetGroup (Maybe Text)
rsgVPCId = lens _rsgVPCId (\s a -> s {_rsgVPCId = a})

-- | The subnets that are in the subnet group.
rsgSubnets :: Lens' ReplicationSubnetGroup [Subnet]
rsgSubnets = lens _rsgSubnets (\s a -> s {_rsgSubnets = a}) . _Default . _Coerce

-- | The identifier of the replication instance subnet group.
rsgReplicationSubnetGroupIdentifier :: Lens' ReplicationSubnetGroup (Maybe Text)
rsgReplicationSubnetGroupIdentifier = lens _rsgReplicationSubnetGroupIdentifier (\s a -> s {_rsgReplicationSubnetGroupIdentifier = a})

-- | The status of the subnet group.
rsgSubnetGroupStatus :: Lens' ReplicationSubnetGroup (Maybe Text)
rsgSubnetGroupStatus = lens _rsgSubnetGroupStatus (\s a -> s {_rsgSubnetGroupStatus = a})

-- | A description for the replication subnet group.
rsgReplicationSubnetGroupDescription :: Lens' ReplicationSubnetGroup (Maybe Text)
rsgReplicationSubnetGroupDescription = lens _rsgReplicationSubnetGroupDescription (\s a -> s {_rsgReplicationSubnetGroupDescription = a})

instance FromJSON ReplicationSubnetGroup where
  parseJSON =
    withObject
      "ReplicationSubnetGroup"
      ( \x ->
          ReplicationSubnetGroup'
            <$> (x .:? "VpcId")
            <*> (x .:? "Subnets" .!= mempty)
            <*> (x .:? "ReplicationSubnetGroupIdentifier")
            <*> (x .:? "SubnetGroupStatus")
            <*> (x .:? "ReplicationSubnetGroupDescription")
      )

instance Hashable ReplicationSubnetGroup

instance NFData ReplicationSubnetGroup
