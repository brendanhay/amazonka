{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.LoggingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.LoggingConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains logging configuration information for a type.
--
--
--
-- /See:/ 'loggingConfig' smart constructor.
data LoggingConfig = LoggingConfig'
  { _lcLogRoleARN :: !Text,
    _lcLogGroupName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoggingConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcLogRoleARN' - The ARN of the role that CloudFormation should assume when sending log entries to CloudWatch logs.
--
-- * 'lcLogGroupName' - The Amazon CloudWatch log group to which CloudFormation sends error logging information when invoking the type's handlers.
loggingConfig ::
  -- | 'lcLogRoleARN'
  Text ->
  -- | 'lcLogGroupName'
  Text ->
  LoggingConfig
loggingConfig pLogRoleARN_ pLogGroupName_ =
  LoggingConfig'
    { _lcLogRoleARN = pLogRoleARN_,
      _lcLogGroupName = pLogGroupName_
    }

-- | The ARN of the role that CloudFormation should assume when sending log entries to CloudWatch logs.
lcLogRoleARN :: Lens' LoggingConfig Text
lcLogRoleARN = lens _lcLogRoleARN (\s a -> s {_lcLogRoleARN = a})

-- | The Amazon CloudWatch log group to which CloudFormation sends error logging information when invoking the type's handlers.
lcLogGroupName :: Lens' LoggingConfig Text
lcLogGroupName = lens _lcLogGroupName (\s a -> s {_lcLogGroupName = a})

instance FromXML LoggingConfig where
  parseXML x =
    LoggingConfig' <$> (x .@ "LogRoleArn") <*> (x .@ "LogGroupName")

instance Hashable LoggingConfig

instance NFData LoggingConfig

instance ToQuery LoggingConfig where
  toQuery LoggingConfig' {..} =
    mconcat
      ["LogRoleArn" =: _lcLogRoleARN, "LogGroupName" =: _lcLogGroupName]
