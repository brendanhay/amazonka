{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.KinesisFirehoseDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.KinesisFirehoseDestination where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the delivery stream ARN and the IAM role ARN associated with an Amazon Kinesis Firehose event destination.
--
--
-- Event destinations, such as Amazon Kinesis Firehose, are associated with configuration sets, which enable you to publish email sending events. For information about using configuration sets, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/monitor-sending-activity.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'kinesisFirehoseDestination' smart constructor.
data KinesisFirehoseDestination = KinesisFirehoseDestination'
  { _kfdIAMRoleARN ::
      !Text,
    _kfdDeliveryStreamARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisFirehoseDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kfdIAMRoleARN' - The ARN of the IAM role under which Amazon SES publishes email sending events to the Amazon Kinesis Firehose stream.
--
-- * 'kfdDeliveryStreamARN' - The ARN of the Amazon Kinesis Firehose stream that email sending events should be published to.
kinesisFirehoseDestination ::
  -- | 'kfdIAMRoleARN'
  Text ->
  -- | 'kfdDeliveryStreamARN'
  Text ->
  KinesisFirehoseDestination
kinesisFirehoseDestination pIAMRoleARN_ pDeliveryStreamARN_ =
  KinesisFirehoseDestination'
    { _kfdIAMRoleARN = pIAMRoleARN_,
      _kfdDeliveryStreamARN = pDeliveryStreamARN_
    }

-- | The ARN of the IAM role under which Amazon SES publishes email sending events to the Amazon Kinesis Firehose stream.
kfdIAMRoleARN :: Lens' KinesisFirehoseDestination Text
kfdIAMRoleARN = lens _kfdIAMRoleARN (\s a -> s {_kfdIAMRoleARN = a})

-- | The ARN of the Amazon Kinesis Firehose stream that email sending events should be published to.
kfdDeliveryStreamARN :: Lens' KinesisFirehoseDestination Text
kfdDeliveryStreamARN = lens _kfdDeliveryStreamARN (\s a -> s {_kfdDeliveryStreamARN = a})

instance FromXML KinesisFirehoseDestination where
  parseXML x =
    KinesisFirehoseDestination'
      <$> (x .@ "IAMRoleARN") <*> (x .@ "DeliveryStreamARN")

instance Hashable KinesisFirehoseDestination

instance NFData KinesisFirehoseDestination

instance ToQuery KinesisFirehoseDestination where
  toQuery KinesisFirehoseDestination' {..} =
    mconcat
      [ "IAMRoleARN" =: _kfdIAMRoleARN,
        "DeliveryStreamARN" =: _kfdDeliveryStreamARN
      ]
