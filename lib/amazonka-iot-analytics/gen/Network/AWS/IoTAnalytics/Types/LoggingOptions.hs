{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.LoggingOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.LoggingOptions where

import Network.AWS.IoTAnalytics.Types.LoggingLevel
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about logging options.
--
--
--
-- /See:/ 'loggingOptions' smart constructor.
data LoggingOptions = LoggingOptions'
  { _loRoleARN :: !Text,
    _loLevel :: !LoggingLevel,
    _loEnabled :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoggingOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loRoleARN' - The ARN of the role that grants permission to AWS IoT Analytics to perform logging.
--
-- * 'loLevel' - The logging level. Currently, only ERROR is supported.
--
-- * 'loEnabled' - If true, logging is enabled for AWS IoT Analytics.
loggingOptions ::
  -- | 'loRoleARN'
  Text ->
  -- | 'loLevel'
  LoggingLevel ->
  -- | 'loEnabled'
  Bool ->
  LoggingOptions
loggingOptions pRoleARN_ pLevel_ pEnabled_ =
  LoggingOptions'
    { _loRoleARN = pRoleARN_,
      _loLevel = pLevel_,
      _loEnabled = pEnabled_
    }

-- | The ARN of the role that grants permission to AWS IoT Analytics to perform logging.
loRoleARN :: Lens' LoggingOptions Text
loRoleARN = lens _loRoleARN (\s a -> s {_loRoleARN = a})

-- | The logging level. Currently, only ERROR is supported.
loLevel :: Lens' LoggingOptions LoggingLevel
loLevel = lens _loLevel (\s a -> s {_loLevel = a})

-- | If true, logging is enabled for AWS IoT Analytics.
loEnabled :: Lens' LoggingOptions Bool
loEnabled = lens _loEnabled (\s a -> s {_loEnabled = a})

instance FromJSON LoggingOptions where
  parseJSON =
    withObject
      "LoggingOptions"
      ( \x ->
          LoggingOptions'
            <$> (x .: "roleArn") <*> (x .: "level") <*> (x .: "enabled")
      )

instance Hashable LoggingOptions

instance NFData LoggingOptions

instance ToJSON LoggingOptions where
  toJSON LoggingOptions' {..} =
    object
      ( catMaybes
          [ Just ("roleArn" .= _loRoleARN),
            Just ("level" .= _loLevel),
            Just ("enabled" .= _loEnabled)
          ]
      )
