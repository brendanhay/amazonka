{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the Amazon Kinesis Firehose delivery stream that is configured as the streaming source in the application input configuration.
--
--
--
-- /See:/ 'kinesisFirehoseInputDescription' smart constructor.
data KinesisFirehoseInputDescription = KinesisFirehoseInputDescription'
  { _kfidResourceARN ::
      !(Maybe Text),
    _kfidRoleARN ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisFirehoseInputDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kfidResourceARN' - Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery stream.
--
-- * 'kfidRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics assumes to access the stream.
kinesisFirehoseInputDescription ::
  KinesisFirehoseInputDescription
kinesisFirehoseInputDescription =
  KinesisFirehoseInputDescription'
    { _kfidResourceARN = Nothing,
      _kfidRoleARN = Nothing
    }

-- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery stream.
kfidResourceARN :: Lens' KinesisFirehoseInputDescription (Maybe Text)
kfidResourceARN = lens _kfidResourceARN (\s a -> s {_kfidResourceARN = a})

-- | ARN of the IAM role that Amazon Kinesis Analytics assumes to access the stream.
kfidRoleARN :: Lens' KinesisFirehoseInputDescription (Maybe Text)
kfidRoleARN = lens _kfidRoleARN (\s a -> s {_kfidRoleARN = a})

instance FromJSON KinesisFirehoseInputDescription where
  parseJSON =
    withObject
      "KinesisFirehoseInputDescription"
      ( \x ->
          KinesisFirehoseInputDescription'
            <$> (x .:? "ResourceARN") <*> (x .:? "RoleARN")
      )

instance Hashable KinesisFirehoseInputDescription

instance NFData KinesisFirehoseInputDescription
