{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterSecurityGroup where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.EC2SecurityGroup
import Network.AWS.Redshift.Types.IPRange
import Network.AWS.Redshift.Types.Tag

-- | Describes a security group.
--
--
--
-- /See:/ 'clusterSecurityGroup' smart constructor.
data ClusterSecurityGroup = ClusterSecurityGroup'
  { _cluClusterSecurityGroupName ::
      !(Maybe Text),
    _cluIPRanges :: !(Maybe [IPRange]),
    _cluEC2SecurityGroups ::
      !(Maybe [EC2SecurityGroup]),
    _cluDescription :: !(Maybe Text),
    _cluTags :: !(Maybe [Tag])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClusterSecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cluClusterSecurityGroupName' - The name of the cluster security group to which the operation was applied.
--
-- * 'cluIPRanges' - A list of IP ranges (CIDR blocks) that are permitted to access clusters associated with this cluster security group.
--
-- * 'cluEC2SecurityGroups' - A list of EC2 security groups that are permitted to access clusters associated with this cluster security group.
--
-- * 'cluDescription' - A description of the security group.
--
-- * 'cluTags' - The list of tags for the cluster security group.
clusterSecurityGroup ::
  ClusterSecurityGroup
clusterSecurityGroup =
  ClusterSecurityGroup'
    { _cluClusterSecurityGroupName = Nothing,
      _cluIPRanges = Nothing,
      _cluEC2SecurityGroups = Nothing,
      _cluDescription = Nothing,
      _cluTags = Nothing
    }

-- | The name of the cluster security group to which the operation was applied.
cluClusterSecurityGroupName :: Lens' ClusterSecurityGroup (Maybe Text)
cluClusterSecurityGroupName = lens _cluClusterSecurityGroupName (\s a -> s {_cluClusterSecurityGroupName = a})

-- | A list of IP ranges (CIDR blocks) that are permitted to access clusters associated with this cluster security group.
cluIPRanges :: Lens' ClusterSecurityGroup [IPRange]
cluIPRanges = lens _cluIPRanges (\s a -> s {_cluIPRanges = a}) . _Default . _Coerce

-- | A list of EC2 security groups that are permitted to access clusters associated with this cluster security group.
cluEC2SecurityGroups :: Lens' ClusterSecurityGroup [EC2SecurityGroup]
cluEC2SecurityGroups = lens _cluEC2SecurityGroups (\s a -> s {_cluEC2SecurityGroups = a}) . _Default . _Coerce

-- | A description of the security group.
cluDescription :: Lens' ClusterSecurityGroup (Maybe Text)
cluDescription = lens _cluDescription (\s a -> s {_cluDescription = a})

-- | The list of tags for the cluster security group.
cluTags :: Lens' ClusterSecurityGroup [Tag]
cluTags = lens _cluTags (\s a -> s {_cluTags = a}) . _Default . _Coerce

instance FromXML ClusterSecurityGroup where
  parseXML x =
    ClusterSecurityGroup'
      <$> (x .@? "ClusterSecurityGroupName")
      <*> (x .@? "IPRanges" .!@ mempty >>= may (parseXMLList "IPRange"))
      <*> ( x .@? "EC2SecurityGroups" .!@ mempty
              >>= may (parseXMLList "EC2SecurityGroup")
          )
      <*> (x .@? "Description")
      <*> (x .@? "Tags" .!@ mempty >>= may (parseXMLList "Tag"))

instance Hashable ClusterSecurityGroup

instance NFData ClusterSecurityGroup
