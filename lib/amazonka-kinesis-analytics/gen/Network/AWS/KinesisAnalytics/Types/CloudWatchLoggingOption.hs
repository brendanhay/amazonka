{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOption where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a description of CloudWatch logging options, including the log stream Amazon Resource Name (ARN) and the role ARN.
--
--
--
-- /See:/ 'cloudWatchLoggingOption' smart constructor.
data CloudWatchLoggingOption = CloudWatchLoggingOption'
  { _cwloLogStreamARN ::
      !Text,
    _cwloRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CloudWatchLoggingOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwloLogStreamARN' - ARN of the CloudWatch log to receive application messages.
--
-- * 'cwloRoleARN' - IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role that is used must have the @PutLogEvents@ policy action enabled.
cloudWatchLoggingOption ::
  -- | 'cwloLogStreamARN'
  Text ->
  -- | 'cwloRoleARN'
  Text ->
  CloudWatchLoggingOption
cloudWatchLoggingOption pLogStreamARN_ pRoleARN_ =
  CloudWatchLoggingOption'
    { _cwloLogStreamARN = pLogStreamARN_,
      _cwloRoleARN = pRoleARN_
    }

-- | ARN of the CloudWatch log to receive application messages.
cwloLogStreamARN :: Lens' CloudWatchLoggingOption Text
cwloLogStreamARN = lens _cwloLogStreamARN (\s a -> s {_cwloLogStreamARN = a})

-- | IAM ARN of the role to use to send application messages. Note: To write application messages to CloudWatch, the IAM role that is used must have the @PutLogEvents@ policy action enabled.
cwloRoleARN :: Lens' CloudWatchLoggingOption Text
cwloRoleARN = lens _cwloRoleARN (\s a -> s {_cwloRoleARN = a})

instance Hashable CloudWatchLoggingOption

instance NFData CloudWatchLoggingOption

instance ToJSON CloudWatchLoggingOption where
  toJSON CloudWatchLoggingOption' {..} =
    object
      ( catMaybes
          [ Just ("LogStreamARN" .= _cwloLogStreamARN),
            Just ("RoleARN" .= _cwloRoleARN)
          ]
      )
