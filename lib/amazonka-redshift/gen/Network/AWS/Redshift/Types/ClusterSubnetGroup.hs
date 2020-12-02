{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterSubnetGroup where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Subnet
import Network.AWS.Redshift.Types.Tag

-- | Describes a subnet group.
--
--
--
-- /See:/ 'clusterSubnetGroup' smart constructor.
data ClusterSubnetGroup = ClusterSubnetGroup'
  { _csgVPCId ::
      !(Maybe Text),
    _csgSubnets :: !(Maybe [Subnet]),
    _csgClusterSubnetGroupName :: !(Maybe Text),
    _csgSubnetGroupStatus :: !(Maybe Text),
    _csgDescription :: !(Maybe Text),
    _csgTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClusterSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csgVPCId' - The VPC ID of the cluster subnet group.
--
-- * 'csgSubnets' - A list of the VPC 'Subnet' elements.
--
-- * 'csgClusterSubnetGroupName' - The name of the cluster subnet group.
--
-- * 'csgSubnetGroupStatus' - The status of the cluster subnet group. Possible values are @Complete@ , @Incomplete@ and @Invalid@ .
--
-- * 'csgDescription' - The description of the cluster subnet group.
--
-- * 'csgTags' - The list of tags for the cluster subnet group.
clusterSubnetGroup ::
  ClusterSubnetGroup
clusterSubnetGroup =
  ClusterSubnetGroup'
    { _csgVPCId = Nothing,
      _csgSubnets = Nothing,
      _csgClusterSubnetGroupName = Nothing,
      _csgSubnetGroupStatus = Nothing,
      _csgDescription = Nothing,
      _csgTags = Nothing
    }

-- | The VPC ID of the cluster subnet group.
csgVPCId :: Lens' ClusterSubnetGroup (Maybe Text)
csgVPCId = lens _csgVPCId (\s a -> s {_csgVPCId = a})

-- | A list of the VPC 'Subnet' elements.
csgSubnets :: Lens' ClusterSubnetGroup [Subnet]
csgSubnets = lens _csgSubnets (\s a -> s {_csgSubnets = a}) . _Default . _Coerce

-- | The name of the cluster subnet group.
csgClusterSubnetGroupName :: Lens' ClusterSubnetGroup (Maybe Text)
csgClusterSubnetGroupName = lens _csgClusterSubnetGroupName (\s a -> s {_csgClusterSubnetGroupName = a})

-- | The status of the cluster subnet group. Possible values are @Complete@ , @Incomplete@ and @Invalid@ .
csgSubnetGroupStatus :: Lens' ClusterSubnetGroup (Maybe Text)
csgSubnetGroupStatus = lens _csgSubnetGroupStatus (\s a -> s {_csgSubnetGroupStatus = a})

-- | The description of the cluster subnet group.
csgDescription :: Lens' ClusterSubnetGroup (Maybe Text)
csgDescription = lens _csgDescription (\s a -> s {_csgDescription = a})

-- | The list of tags for the cluster subnet group.
csgTags :: Lens' ClusterSubnetGroup [Tag]
csgTags = lens _csgTags (\s a -> s {_csgTags = a}) . _Default . _Coerce

instance FromXML ClusterSubnetGroup where
  parseXML x =
    ClusterSubnetGroup'
      <$> (x .@? "VpcId")
      <*> (x .@? "Subnets" .!@ mempty >>= may (parseXMLList "Subnet"))
      <*> (x .@? "ClusterSubnetGroupName")
      <*> (x .@? "SubnetGroupStatus")
      <*> (x .@? "Description")
      <*> (x .@? "Tags" .!@ mempty >>= may (parseXMLList "Tag"))

instance Hashable ClusterSubnetGroup

instance NFData ClusterSubnetGroup
