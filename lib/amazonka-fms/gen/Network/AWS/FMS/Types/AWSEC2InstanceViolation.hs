{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.AWSEC2InstanceViolation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.AWSEC2InstanceViolation where

import Network.AWS.FMS.Types.AWSEC2NetworkInterfaceViolation
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Violations for an EC2 instance resource.
--
--
--
-- /See:/ 'awsEC2InstanceViolation' smart constructor.
data AWSEC2InstanceViolation = AWSEC2InstanceViolation'
  { _aeivViolationTarget ::
      !(Maybe Text),
    _aeivAWSEC2NetworkInterfaceViolations ::
      !(Maybe [AWSEC2NetworkInterfaceViolation])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AWSEC2InstanceViolation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aeivViolationTarget' - The resource ID of the EC2 instance.
--
-- * 'aeivAWSEC2NetworkInterfaceViolations' - Violations for network interfaces associated with the EC2 instance.
awsEC2InstanceViolation ::
  AWSEC2InstanceViolation
awsEC2InstanceViolation =
  AWSEC2InstanceViolation'
    { _aeivViolationTarget = Nothing,
      _aeivAWSEC2NetworkInterfaceViolations = Nothing
    }

-- | The resource ID of the EC2 instance.
aeivViolationTarget :: Lens' AWSEC2InstanceViolation (Maybe Text)
aeivViolationTarget = lens _aeivViolationTarget (\s a -> s {_aeivViolationTarget = a})

-- | Violations for network interfaces associated with the EC2 instance.
aeivAWSEC2NetworkInterfaceViolations :: Lens' AWSEC2InstanceViolation [AWSEC2NetworkInterfaceViolation]
aeivAWSEC2NetworkInterfaceViolations = lens _aeivAWSEC2NetworkInterfaceViolations (\s a -> s {_aeivAWSEC2NetworkInterfaceViolations = a}) . _Default . _Coerce

instance FromJSON AWSEC2InstanceViolation where
  parseJSON =
    withObject
      "AWSEC2InstanceViolation"
      ( \x ->
          AWSEC2InstanceViolation'
            <$> (x .:? "ViolationTarget")
            <*> (x .:? "AwsEc2NetworkInterfaceViolations" .!= mempty)
      )

instance Hashable AWSEC2InstanceViolation

instance NFData AWSEC2InstanceViolation
