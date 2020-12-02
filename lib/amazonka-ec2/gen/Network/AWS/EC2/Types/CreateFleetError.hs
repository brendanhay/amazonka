{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CreateFleetError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CreateFleetError where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceLifecycle
import Network.AWS.EC2.Types.LaunchTemplateAndOverridesResponse
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the instances that could not be launched by the fleet.
--
--
--
-- /See:/ 'createFleetError' smart constructor.
data CreateFleetError = CreateFleetError'
  { _cfeLifecycle ::
      !(Maybe InstanceLifecycle),
    _cfeLaunchTemplateAndOverrides ::
      !(Maybe LaunchTemplateAndOverridesResponse),
    _cfeErrorCode :: !(Maybe Text),
    _cfeErrorMessage :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateFleetError' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfeLifecycle' - Indicates if the instance that could not be launched was a Spot Instance or On-Demand Instance.
--
-- * 'cfeLaunchTemplateAndOverrides' - The launch templates and overrides that were used for launching the instances. The values that you specify in the Overrides replace the values in the launch template.
--
-- * 'cfeErrorCode' - The error code that indicates why the instance could not be launched. For more information about error codes, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
--
-- * 'cfeErrorMessage' - The error message that describes why the instance could not be launched. For more information about error messages, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
createFleetError ::
  CreateFleetError
createFleetError =
  CreateFleetError'
    { _cfeLifecycle = Nothing,
      _cfeLaunchTemplateAndOverrides = Nothing,
      _cfeErrorCode = Nothing,
      _cfeErrorMessage = Nothing
    }

-- | Indicates if the instance that could not be launched was a Spot Instance or On-Demand Instance.
cfeLifecycle :: Lens' CreateFleetError (Maybe InstanceLifecycle)
cfeLifecycle = lens _cfeLifecycle (\s a -> s {_cfeLifecycle = a})

-- | The launch templates and overrides that were used for launching the instances. The values that you specify in the Overrides replace the values in the launch template.
cfeLaunchTemplateAndOverrides :: Lens' CreateFleetError (Maybe LaunchTemplateAndOverridesResponse)
cfeLaunchTemplateAndOverrides = lens _cfeLaunchTemplateAndOverrides (\s a -> s {_cfeLaunchTemplateAndOverrides = a})

-- | The error code that indicates why the instance could not be launched. For more information about error codes, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
cfeErrorCode :: Lens' CreateFleetError (Maybe Text)
cfeErrorCode = lens _cfeErrorCode (\s a -> s {_cfeErrorCode = a})

-- | The error message that describes why the instance could not be launched. For more information about error messages, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html.html Error Codes> .
cfeErrorMessage :: Lens' CreateFleetError (Maybe Text)
cfeErrorMessage = lens _cfeErrorMessage (\s a -> s {_cfeErrorMessage = a})

instance FromXML CreateFleetError where
  parseXML x =
    CreateFleetError'
      <$> (x .@? "lifecycle")
      <*> (x .@? "launchTemplateAndOverrides")
      <*> (x .@? "errorCode")
      <*> (x .@? "errorMessage")

instance Hashable CreateFleetError

instance NFData CreateFleetError
