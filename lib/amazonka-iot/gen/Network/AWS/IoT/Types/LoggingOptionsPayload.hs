{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.LoggingOptionsPayload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.LoggingOptionsPayload where

import Network.AWS.IoT.Types.LogLevel
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the logging options payload.
--
--
--
-- /See:/ 'loggingOptionsPayload' smart constructor.
data LoggingOptionsPayload = LoggingOptionsPayload'
  { _lopLogLevel ::
      !(Maybe LogLevel),
    _lopRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoggingOptionsPayload' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lopLogLevel' - The log level.
--
-- * 'lopRoleARN' - The ARN of the IAM role that grants access.
loggingOptionsPayload ::
  -- | 'lopRoleARN'
  Text ->
  LoggingOptionsPayload
loggingOptionsPayload pRoleARN_ =
  LoggingOptionsPayload'
    { _lopLogLevel = Nothing,
      _lopRoleARN = pRoleARN_
    }

-- | The log level.
lopLogLevel :: Lens' LoggingOptionsPayload (Maybe LogLevel)
lopLogLevel = lens _lopLogLevel (\s a -> s {_lopLogLevel = a})

-- | The ARN of the IAM role that grants access.
lopRoleARN :: Lens' LoggingOptionsPayload Text
lopRoleARN = lens _lopRoleARN (\s a -> s {_lopRoleARN = a})

instance Hashable LoggingOptionsPayload

instance NFData LoggingOptionsPayload

instance ToJSON LoggingOptionsPayload where
  toJSON LoggingOptionsPayload' {..} =
    object
      ( catMaybes
          [ ("logLevel" .=) <$> _lopLogLevel,
            Just ("roleArn" .= _lopRoleARN)
          ]
      )
