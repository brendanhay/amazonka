{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.EC2InstanceLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.EC2InstanceLimit where

import Network.AWS.GameLift.Types.EC2InstanceType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The maximum number of instances allowed based on the Amazon Elastic Compute Cloud (Amazon EC2) instance type. Instance limits can be retrieved by calling 'DescribeEC2InstanceLimits' .
--
--
--
-- /See:/ 'ec2InstanceLimit' smart constructor.
data EC2InstanceLimit = EC2InstanceLimit'
  { _eilEC2InstanceType ::
      !(Maybe EC2InstanceType),
    _eilCurrentInstances :: !(Maybe Nat),
    _eilInstanceLimit :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EC2InstanceLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eilEC2InstanceType' - Name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
--
-- * 'eilCurrentInstances' - Number of instances of the specified type that are currently in use by this AWS account.
--
-- * 'eilInstanceLimit' - Number of instances allowed.
ec2InstanceLimit ::
  EC2InstanceLimit
ec2InstanceLimit =
  EC2InstanceLimit'
    { _eilEC2InstanceType = Nothing,
      _eilCurrentInstances = Nothing,
      _eilInstanceLimit = Nothing
    }

-- | Name of an EC2 instance type that is supported in Amazon GameLift. A fleet instance type determines the computing resources of each instance in the fleet, including CPU, memory, storage, and networking capacity. Amazon GameLift supports the following EC2 instance types. See <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types> for detailed descriptions.
eilEC2InstanceType :: Lens' EC2InstanceLimit (Maybe EC2InstanceType)
eilEC2InstanceType = lens _eilEC2InstanceType (\s a -> s {_eilEC2InstanceType = a})

-- | Number of instances of the specified type that are currently in use by this AWS account.
eilCurrentInstances :: Lens' EC2InstanceLimit (Maybe Natural)
eilCurrentInstances = lens _eilCurrentInstances (\s a -> s {_eilCurrentInstances = a}) . mapping _Nat

-- | Number of instances allowed.
eilInstanceLimit :: Lens' EC2InstanceLimit (Maybe Natural)
eilInstanceLimit = lens _eilInstanceLimit (\s a -> s {_eilInstanceLimit = a}) . mapping _Nat

instance FromJSON EC2InstanceLimit where
  parseJSON =
    withObject
      "EC2InstanceLimit"
      ( \x ->
          EC2InstanceLimit'
            <$> (x .:? "EC2InstanceType")
            <*> (x .:? "CurrentInstances")
            <*> (x .:? "InstanceLimit")
      )

instance Hashable EC2InstanceLimit

instance NFData EC2InstanceLimit
