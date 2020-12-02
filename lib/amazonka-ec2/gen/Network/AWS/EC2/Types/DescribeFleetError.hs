{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DescribeFleetError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DescribeFleetError where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceLifecycle
import Network.AWS.EC2.Types.LaunchTemplateAndOverridesResponse
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the instances that could not be launched by the fleet.
--
--
--
-- /See:/ 'describeFleetError' smart constructor.
data DescribeFleetError = DescribeFleetError'
  { _dfeLifecycle ::
      !(Maybe InstanceLifecycle),
    _dfeLaunchTemplateAndOverrides ::
      !(Maybe LaunchTemplateAndOverridesResponse),
    _dfeErrorCode :: !(Maybe Text),
    _dfeErrorMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeFleetError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfeLifecycle' - Indicates if the instance that could not be launched was a Spot Instance or On-Demand Instance.
--
-- * 'dfeLaunchTemplateAndOverrides' - The launch templates and overrides that were used for launching the instances. The values that you specify in the Overrides replace the values in the launch template.
--
-- * 'dfeErrorCode' - The error code that indicates why the instance could not be launched. For more information about error codes, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
--
-- * 'dfeErrorMessage' - The error message that describes why the instance could not be launched. For more information about error messages, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
describeFleetError ::
  DescribeFleetError
describeFleetError =
  DescribeFleetError'
    { _dfeLifecycle = Nothing,
      _dfeLaunchTemplateAndOverrides = Nothing,
      _dfeErrorCode = Nothing,
      _dfeErrorMessage = Nothing
    }

-- | Indicates if the instance that could not be launched was a Spot Instance or On-Demand Instance.
dfeLifecycle :: Lens' DescribeFleetError (Maybe InstanceLifecycle)
dfeLifecycle = lens _dfeLifecycle (\s a -> s {_dfeLifecycle = a})

-- | The launch templates and overrides that were used for launching the instances. The values that you specify in the Overrides replace the values in the launch template.
dfeLaunchTemplateAndOverrides :: Lens' DescribeFleetError (Maybe LaunchTemplateAndOverridesResponse)
dfeLaunchTemplateAndOverrides = lens _dfeLaunchTemplateAndOverrides (\s a -> s {_dfeLaunchTemplateAndOverrides = a})

-- | The error code that indicates why the instance could not be launched. For more information about error codes, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
dfeErrorCode :: Lens' DescribeFleetError (Maybe Text)
dfeErrorCode = lens _dfeErrorCode (\s a -> s {_dfeErrorCode = a})

-- | The error message that describes why the instance could not be launched. For more information about error messages, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
dfeErrorMessage :: Lens' DescribeFleetError (Maybe Text)
dfeErrorMessage = lens _dfeErrorMessage (\s a -> s {_dfeErrorMessage = a})

instance FromXML DescribeFleetError where
  parseXML x =
    DescribeFleetError'
      <$> (x .@? "lifecycle")
      <*> (x .@? "launchTemplateAndOverrides")
      <*> (x .@? "errorCode")
      <*> (x .@? "errorMessage")

instance Hashable DescribeFleetError

instance NFData DescribeFleetError
