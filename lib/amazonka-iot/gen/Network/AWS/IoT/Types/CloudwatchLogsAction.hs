{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CloudwatchLogsAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CloudwatchLogsAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an action that sends data to CloudWatch Logs.
--
--
--
-- /See:/ 'cloudwatchLogsAction' smart constructor.
data CloudwatchLogsAction = CloudwatchLogsAction'
  { _claRoleARN ::
      !Text,
    _claLogGroupName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudwatchLogsAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'claRoleARN' - The IAM role that allows access to the CloudWatch log.
--
-- * 'claLogGroupName' - The CloudWatch log group to which the action sends data.
cloudwatchLogsAction ::
  -- | 'claRoleARN'
  Text ->
  -- | 'claLogGroupName'
  Text ->
  CloudwatchLogsAction
cloudwatchLogsAction pRoleARN_ pLogGroupName_ =
  CloudwatchLogsAction'
    { _claRoleARN = pRoleARN_,
      _claLogGroupName = pLogGroupName_
    }

-- | The IAM role that allows access to the CloudWatch log.
claRoleARN :: Lens' CloudwatchLogsAction Text
claRoleARN = lens _claRoleARN (\s a -> s {_claRoleARN = a})

-- | The CloudWatch log group to which the action sends data.
claLogGroupName :: Lens' CloudwatchLogsAction Text
claLogGroupName = lens _claLogGroupName (\s a -> s {_claLogGroupName = a})

instance FromJSON CloudwatchLogsAction where
  parseJSON =
    withObject
      "CloudwatchLogsAction"
      ( \x ->
          CloudwatchLogsAction'
            <$> (x .: "roleArn") <*> (x .: "logGroupName")
      )

instance Hashable CloudwatchLogsAction

instance NFData CloudwatchLogsAction

instance ToJSON CloudwatchLogsAction where
  toJSON CloudwatchLogsAction' {..} =
    object
      ( catMaybes
          [ Just ("roleArn" .= _claRoleARN),
            Just ("logGroupName" .= _claLogGroupName)
          ]
      )
