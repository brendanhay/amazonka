{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.SqsParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.SqsParameters where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | This structure includes the custom parameter to be used when the target is an SQS FIFO queue.
--
--
--
-- /See:/ 'sqsParameters' smart constructor.
newtype SqsParameters = SqsParameters'
  { _spMessageGroupId ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SqsParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spMessageGroupId' - The FIFO message group ID to use as the target.
sqsParameters ::
  SqsParameters
sqsParameters = SqsParameters' {_spMessageGroupId = Nothing}

-- | The FIFO message group ID to use as the target.
spMessageGroupId :: Lens' SqsParameters (Maybe Text)
spMessageGroupId = lens _spMessageGroupId (\s a -> s {_spMessageGroupId = a})

instance FromJSON SqsParameters where
  parseJSON =
    withObject
      "SqsParameters"
      (\x -> SqsParameters' <$> (x .:? "MessageGroupId"))

instance Hashable SqsParameters

instance NFData SqsParameters

instance ToJSON SqsParameters where
  toJSON SqsParameters' {..} =
    object (catMaybes [("MessageGroupId" .=) <$> _spMessageGroupId])
