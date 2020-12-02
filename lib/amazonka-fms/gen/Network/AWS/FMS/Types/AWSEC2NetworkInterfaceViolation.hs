{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.AWSEC2NetworkInterfaceViolation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.AWSEC2NetworkInterfaceViolation where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Violations for network interfaces associated with an EC2 instance.
--
--
--
-- /See:/ 'awsEC2NetworkInterfaceViolation' smart constructor.
data AWSEC2NetworkInterfaceViolation = AWSEC2NetworkInterfaceViolation'
  { _aenivViolatingSecurityGroups ::
      !(Maybe [Text]),
    _aenivViolationTarget ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AWSEC2NetworkInterfaceViolation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aenivViolatingSecurityGroups' - List of security groups that violate the rules specified in the master security group of the AWS Firewall Manager policy.
--
-- * 'aenivViolationTarget' - The resource ID of the network interface.
awsEC2NetworkInterfaceViolation ::
  AWSEC2NetworkInterfaceViolation
awsEC2NetworkInterfaceViolation =
  AWSEC2NetworkInterfaceViolation'
    { _aenivViolatingSecurityGroups =
        Nothing,
      _aenivViolationTarget = Nothing
    }

-- | List of security groups that violate the rules specified in the master security group of the AWS Firewall Manager policy.
aenivViolatingSecurityGroups :: Lens' AWSEC2NetworkInterfaceViolation [Text]
aenivViolatingSecurityGroups = lens _aenivViolatingSecurityGroups (\s a -> s {_aenivViolatingSecurityGroups = a}) . _Default . _Coerce

-- | The resource ID of the network interface.
aenivViolationTarget :: Lens' AWSEC2NetworkInterfaceViolation (Maybe Text)
aenivViolationTarget = lens _aenivViolationTarget (\s a -> s {_aenivViolationTarget = a})

instance FromJSON AWSEC2NetworkInterfaceViolation where
  parseJSON =
    withObject
      "AWSEC2NetworkInterfaceViolation"
      ( \x ->
          AWSEC2NetworkInterfaceViolation'
            <$> (x .:? "ViolatingSecurityGroups" .!= mempty)
            <*> (x .:? "ViolationTarget")
      )

instance Hashable AWSEC2NetworkInterfaceViolation

instance NFData AWSEC2NetworkInterfaceViolation
