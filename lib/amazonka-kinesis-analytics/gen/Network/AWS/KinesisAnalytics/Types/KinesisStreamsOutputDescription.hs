{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | For an application output, describes the Amazon Kinesis stream configured as its destination.
--
--
--
-- /See:/ 'kinesisStreamsOutputDescription' smart constructor.
data KinesisStreamsOutputDescription = KinesisStreamsOutputDescription'
  { _ksodResourceARN ::
      !(Maybe Text),
    _ksodRoleARN ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisStreamsOutputDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ksodResourceARN' - Amazon Resource Name (ARN) of the Amazon Kinesis stream.
--
-- * 'ksodRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
kinesisStreamsOutputDescription ::
  KinesisStreamsOutputDescription
kinesisStreamsOutputDescription =
  KinesisStreamsOutputDescription'
    { _ksodResourceARN = Nothing,
      _ksodRoleARN = Nothing
    }

-- | Amazon Resource Name (ARN) of the Amazon Kinesis stream.
ksodResourceARN :: Lens' KinesisStreamsOutputDescription (Maybe Text)
ksodResourceARN = lens _ksodResourceARN (\s a -> s {_ksodResourceARN = a})

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
ksodRoleARN :: Lens' KinesisStreamsOutputDescription (Maybe Text)
ksodRoleARN = lens _ksodRoleARN (\s a -> s {_ksodRoleARN = a})

instance FromJSON KinesisStreamsOutputDescription where
  parseJSON =
    withObject
      "KinesisStreamsOutputDescription"
      ( \x ->
          KinesisStreamsOutputDescription'
            <$> (x .:? "ResourceARN") <*> (x .:? "RoleARN")
      )

instance Hashable KinesisStreamsOutputDescription

instance NFData KinesisStreamsOutputDescription
