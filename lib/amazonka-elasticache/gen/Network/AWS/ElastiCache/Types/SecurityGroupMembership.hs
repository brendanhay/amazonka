{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.SecurityGroupMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.SecurityGroupMembership where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a single cache security group and its status.
--
--
--
-- /See:/ 'securityGroupMembership' smart constructor.
data SecurityGroupMembership = SecurityGroupMembership'
  { _sgmStatus ::
      !(Maybe Text),
    _sgmSecurityGroupId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SecurityGroupMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgmStatus' - The status of the cache security group membership. The status changes whenever a cache security group is modified, or when the cache security groups assigned to a cluster are modified.
--
-- * 'sgmSecurityGroupId' - The identifier of the cache security group.
securityGroupMembership ::
  SecurityGroupMembership
securityGroupMembership =
  SecurityGroupMembership'
    { _sgmStatus = Nothing,
      _sgmSecurityGroupId = Nothing
    }

-- | The status of the cache security group membership. The status changes whenever a cache security group is modified, or when the cache security groups assigned to a cluster are modified.
sgmStatus :: Lens' SecurityGroupMembership (Maybe Text)
sgmStatus = lens _sgmStatus (\s a -> s {_sgmStatus = a})

-- | The identifier of the cache security group.
sgmSecurityGroupId :: Lens' SecurityGroupMembership (Maybe Text)
sgmSecurityGroupId = lens _sgmSecurityGroupId (\s a -> s {_sgmSecurityGroupId = a})

instance FromXML SecurityGroupMembership where
  parseXML x =
    SecurityGroupMembership'
      <$> (x .@? "Status") <*> (x .@? "SecurityGroupId")

instance Hashable SecurityGroupMembership

instance NFData SecurityGroupMembership
