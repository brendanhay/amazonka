{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.SecurityGroupMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.SecurityGroupMembership where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An individual VPC security group and its status.
--
--
--
-- /See:/ 'securityGroupMembership' smart constructor.
data SecurityGroupMembership = SecurityGroupMembership'
  { _sgmStatus ::
      !(Maybe Text),
    _sgmSecurityGroupIdentifier ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SecurityGroupMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgmStatus' - The status of this security group.
--
-- * 'sgmSecurityGroupIdentifier' - The unique ID for this security group.
securityGroupMembership ::
  SecurityGroupMembership
securityGroupMembership =
  SecurityGroupMembership'
    { _sgmStatus = Nothing,
      _sgmSecurityGroupIdentifier = Nothing
    }

-- | The status of this security group.
sgmStatus :: Lens' SecurityGroupMembership (Maybe Text)
sgmStatus = lens _sgmStatus (\s a -> s {_sgmStatus = a})

-- | The unique ID for this security group.
sgmSecurityGroupIdentifier :: Lens' SecurityGroupMembership (Maybe Text)
sgmSecurityGroupIdentifier = lens _sgmSecurityGroupIdentifier (\s a -> s {_sgmSecurityGroupIdentifier = a})

instance FromJSON SecurityGroupMembership where
  parseJSON =
    withObject
      "SecurityGroupMembership"
      ( \x ->
          SecurityGroupMembership'
            <$> (x .:? "Status") <*> (x .:? "SecurityGroupIdentifier")
      )

instance Hashable SecurityGroupMembership

instance NFData SecurityGroupMembership
