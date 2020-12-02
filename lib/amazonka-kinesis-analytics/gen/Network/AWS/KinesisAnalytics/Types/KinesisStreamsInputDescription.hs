{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the Amazon Kinesis stream that is configured as the streaming source in the application input configuration.
--
--
--
-- /See:/ 'kinesisStreamsInputDescription' smart constructor.
data KinesisStreamsInputDescription = KinesisStreamsInputDescription'
  { _ksidResourceARN ::
      !(Maybe Text),
    _ksidRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisStreamsInputDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ksidResourceARN' - Amazon Resource Name (ARN) of the Amazon Kinesis stream.
--
-- * 'ksidRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
kinesisStreamsInputDescription ::
  KinesisStreamsInputDescription
kinesisStreamsInputDescription =
  KinesisStreamsInputDescription'
    { _ksidResourceARN = Nothing,
      _ksidRoleARN = Nothing
    }

-- | Amazon Resource Name (ARN) of the Amazon Kinesis stream.
ksidResourceARN :: Lens' KinesisStreamsInputDescription (Maybe Text)
ksidResourceARN = lens _ksidResourceARN (\s a -> s {_ksidResourceARN = a})

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
ksidRoleARN :: Lens' KinesisStreamsInputDescription (Maybe Text)
ksidRoleARN = lens _ksidRoleARN (\s a -> s {_ksidRoleARN = a})

instance FromJSON KinesisStreamsInputDescription where
  parseJSON =
    withObject
      "KinesisStreamsInputDescription"
      ( \x ->
          KinesisStreamsInputDescription'
            <$> (x .:? "ResourceARN") <*> (x .:? "RoleARN")
      )

instance Hashable KinesisStreamsInputDescription

instance NFData KinesisStreamsInputDescription
