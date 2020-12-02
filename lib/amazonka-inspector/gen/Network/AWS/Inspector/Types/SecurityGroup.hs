{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.SecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.SecurityGroup where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a security group associated with a network interface. This data type is used as one of the elements of the 'NetworkInterface' data type.
--
--
--
-- /See:/ 'securityGroup' smart constructor.
data SecurityGroup = SecurityGroup'
  { _sgGroupId :: !(Maybe Text),
    _sgGroupName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgGroupId' - The ID of the security group.
--
-- * 'sgGroupName' - The name of the security group.
securityGroup ::
  SecurityGroup
securityGroup =
  SecurityGroup' {_sgGroupId = Nothing, _sgGroupName = Nothing}

-- | The ID of the security group.
sgGroupId :: Lens' SecurityGroup (Maybe Text)
sgGroupId = lens _sgGroupId (\s a -> s {_sgGroupId = a})

-- | The name of the security group.
sgGroupName :: Lens' SecurityGroup (Maybe Text)
sgGroupName = lens _sgGroupName (\s a -> s {_sgGroupName = a})

instance FromJSON SecurityGroup where
  parseJSON =
    withObject
      "SecurityGroup"
      ( \x ->
          SecurityGroup' <$> (x .:? "groupId") <*> (x .:? "groupName")
      )

instance Hashable SecurityGroup

instance NFData SecurityGroup
