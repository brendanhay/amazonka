{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.LogDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LogDestination where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.StepFunctions.Types.CloudWatchLogsLogGroup

-- |
--
--
--
-- /See:/ 'logDestination' smart constructor.
newtype LogDestination = LogDestination'
  { _ldCloudWatchLogsLogGroup ::
      Maybe CloudWatchLogsLogGroup
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LogDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldCloudWatchLogsLogGroup' - An object describing a CloudWatch log group. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-logs-loggroup.html AWS::Logs::LogGroup> in the AWS CloudFormation User Guide.
logDestination ::
  LogDestination
logDestination =
  LogDestination' {_ldCloudWatchLogsLogGroup = Nothing}

-- | An object describing a CloudWatch log group. For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-logs-loggroup.html AWS::Logs::LogGroup> in the AWS CloudFormation User Guide.
ldCloudWatchLogsLogGroup :: Lens' LogDestination (Maybe CloudWatchLogsLogGroup)
ldCloudWatchLogsLogGroup = lens _ldCloudWatchLogsLogGroup (\s a -> s {_ldCloudWatchLogsLogGroup = a})

instance FromJSON LogDestination where
  parseJSON =
    withObject
      "LogDestination"
      (\x -> LogDestination' <$> (x .:? "cloudWatchLogsLogGroup"))

instance Hashable LogDestination

instance NFData LogDestination

instance ToJSON LogDestination where
  toJSON LogDestination' {..} =
    object
      ( catMaybes
          [("cloudWatchLogsLogGroup" .=) <$> _ldCloudWatchLogsLogGroup]
      )
