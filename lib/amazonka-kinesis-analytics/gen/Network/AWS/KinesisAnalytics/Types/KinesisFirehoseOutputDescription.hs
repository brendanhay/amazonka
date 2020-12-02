{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | For an application output, describes the Amazon Kinesis Firehose delivery stream configured as its destination.
--
--
--
-- /See:/ 'kinesisFirehoseOutputDescription' smart constructor.
data KinesisFirehoseOutputDescription = KinesisFirehoseOutputDescription'
  { _kfodResourceARN ::
      !(Maybe Text),
    _kfodRoleARN ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisFirehoseOutputDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kfodResourceARN' - Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery stream.
--
-- * 'kfodRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
kinesisFirehoseOutputDescription ::
  KinesisFirehoseOutputDescription
kinesisFirehoseOutputDescription =
  KinesisFirehoseOutputDescription'
    { _kfodResourceARN = Nothing,
      _kfodRoleARN = Nothing
    }

-- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery stream.
kfodResourceARN :: Lens' KinesisFirehoseOutputDescription (Maybe Text)
kfodResourceARN = lens _kfodResourceARN (\s a -> s {_kfodResourceARN = a})

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
kfodRoleARN :: Lens' KinesisFirehoseOutputDescription (Maybe Text)
kfodRoleARN = lens _kfodRoleARN (\s a -> s {_kfodRoleARN = a})

instance FromJSON KinesisFirehoseOutputDescription where
  parseJSON =
    withObject
      "KinesisFirehoseOutputDescription"
      ( \x ->
          KinesisFirehoseOutputDescription'
            <$> (x .:? "ResourceARN") <*> (x .:? "RoleARN")
      )

instance Hashable KinesisFirehoseOutputDescription

instance NFData KinesisFirehoseOutputDescription
