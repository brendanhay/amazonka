{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.FunctionConfigurationEnvironment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionConfigurationEnvironment where

import Network.AWS.Greengrass.Types.FunctionExecutionConfig
import Network.AWS.Greengrass.Types.ResourceAccessPolicy
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The environment configuration of the function.
--
-- /See:/ 'functionConfigurationEnvironment' smart constructor.
data FunctionConfigurationEnvironment = FunctionConfigurationEnvironment'
  { _fceVariables ::
      !( Maybe
           (Map Text (Text))
       ),
    _fceExecution ::
      !( Maybe
           FunctionExecutionConfig
       ),
    _fceResourceAccessPolicies ::
      !( Maybe
           [ResourceAccessPolicy]
       ),
    _fceAccessSysfs ::
      !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FunctionConfigurationEnvironment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fceVariables' - Environment variables for the Lambda function's configuration.
--
-- * 'fceExecution' - Configuration related to executing the Lambda function
--
-- * 'fceResourceAccessPolicies' - A list of the resources, with their permissions, to which the Lambda function will be granted access. A Lambda function can have at most 10 resources. ResourceAccessPolicies apply only when you run the Lambda function in a Greengrass container.
--
-- * 'fceAccessSysfs' - If true, the Lambda function is allowed to access the host's /sys folder. Use this when the Lambda function needs to read device information from /sys. This setting applies only when you run the Lambda function in a Greengrass container.
functionConfigurationEnvironment ::
  FunctionConfigurationEnvironment
functionConfigurationEnvironment =
  FunctionConfigurationEnvironment'
    { _fceVariables = Nothing,
      _fceExecution = Nothing,
      _fceResourceAccessPolicies = Nothing,
      _fceAccessSysfs = Nothing
    }

-- | Environment variables for the Lambda function's configuration.
fceVariables :: Lens' FunctionConfigurationEnvironment (HashMap Text (Text))
fceVariables = lens _fceVariables (\s a -> s {_fceVariables = a}) . _Default . _Map

-- | Configuration related to executing the Lambda function
fceExecution :: Lens' FunctionConfigurationEnvironment (Maybe FunctionExecutionConfig)
fceExecution = lens _fceExecution (\s a -> s {_fceExecution = a})

-- | A list of the resources, with their permissions, to which the Lambda function will be granted access. A Lambda function can have at most 10 resources. ResourceAccessPolicies apply only when you run the Lambda function in a Greengrass container.
fceResourceAccessPolicies :: Lens' FunctionConfigurationEnvironment [ResourceAccessPolicy]
fceResourceAccessPolicies = lens _fceResourceAccessPolicies (\s a -> s {_fceResourceAccessPolicies = a}) . _Default . _Coerce

-- | If true, the Lambda function is allowed to access the host's /sys folder. Use this when the Lambda function needs to read device information from /sys. This setting applies only when you run the Lambda function in a Greengrass container.
fceAccessSysfs :: Lens' FunctionConfigurationEnvironment (Maybe Bool)
fceAccessSysfs = lens _fceAccessSysfs (\s a -> s {_fceAccessSysfs = a})

instance FromJSON FunctionConfigurationEnvironment where
  parseJSON =
    withObject
      "FunctionConfigurationEnvironment"
      ( \x ->
          FunctionConfigurationEnvironment'
            <$> (x .:? "Variables" .!= mempty)
            <*> (x .:? "Execution")
            <*> (x .:? "ResourceAccessPolicies" .!= mempty)
            <*> (x .:? "AccessSysfs")
      )

instance Hashable FunctionConfigurationEnvironment

instance NFData FunctionConfigurationEnvironment

instance ToJSON FunctionConfigurationEnvironment where
  toJSON FunctionConfigurationEnvironment' {..} =
    object
      ( catMaybes
          [ ("Variables" .=) <$> _fceVariables,
            ("Execution" .=) <$> _fceExecution,
            ("ResourceAccessPolicies" .=) <$> _fceResourceAccessPolicies,
            ("AccessSysfs" .=) <$> _fceAccessSysfs
          ]
      )
