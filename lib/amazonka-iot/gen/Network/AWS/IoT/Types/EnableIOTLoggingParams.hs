{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.EnableIOTLoggingParams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.EnableIOTLoggingParams where

import Network.AWS.IoT.Types.LogLevel
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Parameters used when defining a mitigation action that enable AWS IoT logging.
--
--
--
-- /See:/ 'enableIOTLoggingParams' smart constructor.
data EnableIOTLoggingParams = EnableIOTLoggingParams'
  { _eiotlpRoleARNForLogging ::
      !Text,
    _eiotlpLogLevel :: !LogLevel
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnableIOTLoggingParams' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eiotlpRoleARNForLogging' - The ARN of the IAM role used for logging.
--
-- * 'eiotlpLogLevel' - Specifies the types of information to be logged.
enableIOTLoggingParams ::
  -- | 'eiotlpRoleARNForLogging'
  Text ->
  -- | 'eiotlpLogLevel'
  LogLevel ->
  EnableIOTLoggingParams
enableIOTLoggingParams pRoleARNForLogging_ pLogLevel_ =
  EnableIOTLoggingParams'
    { _eiotlpRoleARNForLogging =
        pRoleARNForLogging_,
      _eiotlpLogLevel = pLogLevel_
    }

-- | The ARN of the IAM role used for logging.
eiotlpRoleARNForLogging :: Lens' EnableIOTLoggingParams Text
eiotlpRoleARNForLogging = lens _eiotlpRoleARNForLogging (\s a -> s {_eiotlpRoleARNForLogging = a})

-- | Specifies the types of information to be logged.
eiotlpLogLevel :: Lens' EnableIOTLoggingParams LogLevel
eiotlpLogLevel = lens _eiotlpLogLevel (\s a -> s {_eiotlpLogLevel = a})

instance FromJSON EnableIOTLoggingParams where
  parseJSON =
    withObject
      "EnableIOTLoggingParams"
      ( \x ->
          EnableIOTLoggingParams'
            <$> (x .: "roleArnForLogging") <*> (x .: "logLevel")
      )

instance Hashable EnableIOTLoggingParams

instance NFData EnableIOTLoggingParams

instance ToJSON EnableIOTLoggingParams where
  toJSON EnableIOTLoggingParams' {..} =
    object
      ( catMaybes
          [ Just ("roleArnForLogging" .= _eiotlpRoleARNForLogging),
            Just ("logLevel" .= _eiotlpLogLevel)
          ]
      )
