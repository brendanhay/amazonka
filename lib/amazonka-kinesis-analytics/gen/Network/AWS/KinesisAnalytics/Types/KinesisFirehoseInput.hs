{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInput where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Identifies an Amazon Kinesis Firehose delivery stream as the streaming source. You provide the delivery stream's Amazon Resource Name (ARN) and an IAM role ARN that enables Amazon Kinesis Analytics to access the stream on your behalf.
--
--
--
-- /See:/ 'kinesisFirehoseInput' smart constructor.
data KinesisFirehoseInput = KinesisFirehoseInput'
  { _kfiResourceARN ::
      !Text,
    _kfiRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisFirehoseInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kfiResourceARN' - ARN of the input delivery stream.
--
-- * 'kfiRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to make sure that the role has the necessary permissions to access the stream.
kinesisFirehoseInput ::
  -- | 'kfiResourceARN'
  Text ->
  -- | 'kfiRoleARN'
  Text ->
  KinesisFirehoseInput
kinesisFirehoseInput pResourceARN_ pRoleARN_ =
  KinesisFirehoseInput'
    { _kfiResourceARN = pResourceARN_,
      _kfiRoleARN = pRoleARN_
    }

-- | ARN of the input delivery stream.
kfiResourceARN :: Lens' KinesisFirehoseInput Text
kfiResourceARN = lens _kfiResourceARN (\s a -> s {_kfiResourceARN = a})

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to make sure that the role has the necessary permissions to access the stream.
kfiRoleARN :: Lens' KinesisFirehoseInput Text
kfiRoleARN = lens _kfiRoleARN (\s a -> s {_kfiRoleARN = a})

instance Hashable KinesisFirehoseInput

instance NFData KinesisFirehoseInput

instance ToJSON KinesisFirehoseInput where
  toJSON KinesisFirehoseInput' {..} =
    object
      ( catMaybes
          [ Just ("ResourceARN" .= _kfiResourceARN),
            Just ("RoleARN" .= _kfiRoleARN)
          ]
      )
