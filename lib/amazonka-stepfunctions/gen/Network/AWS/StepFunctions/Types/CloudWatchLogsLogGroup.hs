{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.CloudWatchLogsLogGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.CloudWatchLogsLogGroup where

import Network.AWS.Lens
import Network.AWS.Prelude

-- |
--
--
--
-- /See:/ 'cloudWatchLogsLogGroup' smart constructor.
newtype CloudWatchLogsLogGroup = CloudWatchLogsLogGroup'
  { _cwllgLogGroupARN ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudWatchLogsLogGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwllgLogGroupARN' - The ARN of the the CloudWatch log group to which you want your logs emitted to. The ARN must end with @:*@
cloudWatchLogsLogGroup ::
  CloudWatchLogsLogGroup
cloudWatchLogsLogGroup =
  CloudWatchLogsLogGroup' {_cwllgLogGroupARN = Nothing}

-- | The ARN of the the CloudWatch log group to which you want your logs emitted to. The ARN must end with @:*@
cwllgLogGroupARN :: Lens' CloudWatchLogsLogGroup (Maybe Text)
cwllgLogGroupARN = lens _cwllgLogGroupARN (\s a -> s {_cwllgLogGroupARN = a})

instance FromJSON CloudWatchLogsLogGroup where
  parseJSON =
    withObject
      "CloudWatchLogsLogGroup"
      (\x -> CloudWatchLogsLogGroup' <$> (x .:? "logGroupArn"))

instance Hashable CloudWatchLogsLogGroup

instance NFData CloudWatchLogsLogGroup

instance ToJSON CloudWatchLogsLogGroup where
  toJSON CloudWatchLogsLogGroup' {..} =
    object (catMaybes [("logGroupArn" .=) <$> _cwllgLogGroupARN])
