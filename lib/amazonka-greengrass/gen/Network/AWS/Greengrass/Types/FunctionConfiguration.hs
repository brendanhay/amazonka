{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.FunctionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionConfiguration where

import Network.AWS.Greengrass.Types.EncodingType
import Network.AWS.Greengrass.Types.FunctionConfigurationEnvironment
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The configuration of the Lambda function.
--
-- /See:/ 'functionConfiguration' smart constructor.
data FunctionConfiguration = FunctionConfiguration'
  { _fcMemorySize ::
      !(Maybe Int),
    _fcExecArgs :: !(Maybe Text),
    _fcEnvironment ::
      !(Maybe FunctionConfigurationEnvironment),
    _fcExecutable :: !(Maybe Text),
    _fcPinned :: !(Maybe Bool),
    _fcEncodingType :: !(Maybe EncodingType),
    _fcTimeout :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FunctionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcMemorySize' - The memory size, in KB, which the function requires. This setting is not applicable and should be cleared when you run the Lambda function without containerization.
--
-- * 'fcExecArgs' - The execution arguments.
--
-- * 'fcEnvironment' - The environment configuration of the function.
--
-- * 'fcExecutable' - The name of the function executable.
--
-- * 'fcPinned' - True if the function is pinned. Pinned means the function is long-lived and starts when the core starts.
--
-- * 'fcEncodingType' - The expected encoding type of the input payload for the function. The default is ''json''.
--
-- * 'fcTimeout' - The allowed function execution time, after which Lambda should terminate the function. This timeout still applies to pinned Lambda functions for each request.
functionConfiguration ::
  FunctionConfiguration
functionConfiguration =
  FunctionConfiguration'
    { _fcMemorySize = Nothing,
      _fcExecArgs = Nothing,
      _fcEnvironment = Nothing,
      _fcExecutable = Nothing,
      _fcPinned = Nothing,
      _fcEncodingType = Nothing,
      _fcTimeout = Nothing
    }

-- | The memory size, in KB, which the function requires. This setting is not applicable and should be cleared when you run the Lambda function without containerization.
fcMemorySize :: Lens' FunctionConfiguration (Maybe Int)
fcMemorySize = lens _fcMemorySize (\s a -> s {_fcMemorySize = a})

-- | The execution arguments.
fcExecArgs :: Lens' FunctionConfiguration (Maybe Text)
fcExecArgs = lens _fcExecArgs (\s a -> s {_fcExecArgs = a})

-- | The environment configuration of the function.
fcEnvironment :: Lens' FunctionConfiguration (Maybe FunctionConfigurationEnvironment)
fcEnvironment = lens _fcEnvironment (\s a -> s {_fcEnvironment = a})

-- | The name of the function executable.
fcExecutable :: Lens' FunctionConfiguration (Maybe Text)
fcExecutable = lens _fcExecutable (\s a -> s {_fcExecutable = a})

-- | True if the function is pinned. Pinned means the function is long-lived and starts when the core starts.
fcPinned :: Lens' FunctionConfiguration (Maybe Bool)
fcPinned = lens _fcPinned (\s a -> s {_fcPinned = a})

-- | The expected encoding type of the input payload for the function. The default is ''json''.
fcEncodingType :: Lens' FunctionConfiguration (Maybe EncodingType)
fcEncodingType = lens _fcEncodingType (\s a -> s {_fcEncodingType = a})

-- | The allowed function execution time, after which Lambda should terminate the function. This timeout still applies to pinned Lambda functions for each request.
fcTimeout :: Lens' FunctionConfiguration (Maybe Int)
fcTimeout = lens _fcTimeout (\s a -> s {_fcTimeout = a})

instance FromJSON FunctionConfiguration where
  parseJSON =
    withObject
      "FunctionConfiguration"
      ( \x ->
          FunctionConfiguration'
            <$> (x .:? "MemorySize")
            <*> (x .:? "ExecArgs")
            <*> (x .:? "Environment")
            <*> (x .:? "Executable")
            <*> (x .:? "Pinned")
            <*> (x .:? "EncodingType")
            <*> (x .:? "Timeout")
      )

instance Hashable FunctionConfiguration

instance NFData FunctionConfiguration

instance ToJSON FunctionConfiguration where
  toJSON FunctionConfiguration' {..} =
    object
      ( catMaybes
          [ ("MemorySize" .=) <$> _fcMemorySize,
            ("ExecArgs" .=) <$> _fcExecArgs,
            ("Environment" .=) <$> _fcEnvironment,
            ("Executable" .=) <$> _fcExecutable,
            ("Pinned" .=) <$> _fcPinned,
            ("EncodingType" .=) <$> _fcEncodingType,
            ("Timeout" .=) <$> _fcTimeout
          ]
      )
