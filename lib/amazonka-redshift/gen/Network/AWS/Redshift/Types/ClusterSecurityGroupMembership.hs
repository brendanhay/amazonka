{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterSecurityGroupMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterSecurityGroupMembership where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

-- | Describes a cluster security group.
--
--
--
-- /See:/ 'clusterSecurityGroupMembership' smart constructor.
data ClusterSecurityGroupMembership = ClusterSecurityGroupMembership'
  { _csgmStatus ::
      !(Maybe Text),
    _csgmClusterSecurityGroupName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClusterSecurityGroupMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csgmStatus' - The status of the cluster security group.
--
-- * 'csgmClusterSecurityGroupName' - The name of the cluster security group.
clusterSecurityGroupMembership ::
  ClusterSecurityGroupMembership
clusterSecurityGroupMembership =
  ClusterSecurityGroupMembership'
    { _csgmStatus = Nothing,
      _csgmClusterSecurityGroupName = Nothing
    }

-- | The status of the cluster security group.
csgmStatus :: Lens' ClusterSecurityGroupMembership (Maybe Text)
csgmStatus = lens _csgmStatus (\s a -> s {_csgmStatus = a})

-- | The name of the cluster security group.
csgmClusterSecurityGroupName :: Lens' ClusterSecurityGroupMembership (Maybe Text)
csgmClusterSecurityGroupName = lens _csgmClusterSecurityGroupName (\s a -> s {_csgmClusterSecurityGroupName = a})

instance FromXML ClusterSecurityGroupMembership where
  parseXML x =
    ClusterSecurityGroupMembership'
      <$> (x .@? "Status") <*> (x .@? "ClusterSecurityGroupName")

instance Hashable ClusterSecurityGroupMembership

instance NFData ClusterSecurityGroupMembership
