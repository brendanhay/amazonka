{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SecurityGroupIdentifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SecurityGroupIdentifier where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a security group.
--
--
--
-- /See:/ 'securityGroupIdentifier' smart constructor.
data SecurityGroupIdentifier = SecurityGroupIdentifier'
  { _sgiGroupId ::
      !(Maybe Text),
    _sgiGroupName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SecurityGroupIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgiGroupId' - The ID of the security group.
--
-- * 'sgiGroupName' - The name of the security group.
securityGroupIdentifier ::
  SecurityGroupIdentifier
securityGroupIdentifier =
  SecurityGroupIdentifier'
    { _sgiGroupId = Nothing,
      _sgiGroupName = Nothing
    }

-- | The ID of the security group.
sgiGroupId :: Lens' SecurityGroupIdentifier (Maybe Text)
sgiGroupId = lens _sgiGroupId (\s a -> s {_sgiGroupId = a})

-- | The name of the security group.
sgiGroupName :: Lens' SecurityGroupIdentifier (Maybe Text)
sgiGroupName = lens _sgiGroupName (\s a -> s {_sgiGroupName = a})

instance FromXML SecurityGroupIdentifier where
  parseXML x =
    SecurityGroupIdentifier'
      <$> (x .@? "groupId") <*> (x .@? "groupName")

instance Hashable SecurityGroupIdentifier

instance NFData SecurityGroupIdentifier
