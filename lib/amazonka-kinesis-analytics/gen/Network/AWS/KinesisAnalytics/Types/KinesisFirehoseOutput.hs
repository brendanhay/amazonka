{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutput where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | When configuring application output, identifies an Amazon Kinesis Firehose delivery stream as the destination. You provide the stream Amazon Resource Name (ARN) and an IAM role that enables Amazon Kinesis Analytics to write to the stream on your behalf.
--
--
--
-- /See:/ 'kinesisFirehoseOutput' smart constructor.
data KinesisFirehoseOutput = KinesisFirehoseOutput'
  { _kfoResourceARN ::
      !Text,
    _kfoRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisFirehoseOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kfoResourceARN' - ARN of the destination Amazon Kinesis Firehose delivery stream to write to.
--
-- * 'kfoRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination stream on your behalf. You need to grant the necessary permissions to this role.
kinesisFirehoseOutput ::
  -- | 'kfoResourceARN'
  Text ->
  -- | 'kfoRoleARN'
  Text ->
  KinesisFirehoseOutput
kinesisFirehoseOutput pResourceARN_ pRoleARN_ =
  KinesisFirehoseOutput'
    { _kfoResourceARN = pResourceARN_,
      _kfoRoleARN = pRoleARN_
    }

-- | ARN of the destination Amazon Kinesis Firehose delivery stream to write to.
kfoResourceARN :: Lens' KinesisFirehoseOutput Text
kfoResourceARN = lens _kfoResourceARN (\s a -> s {_kfoResourceARN = a})

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination stream on your behalf. You need to grant the necessary permissions to this role.
kfoRoleARN :: Lens' KinesisFirehoseOutput Text
kfoRoleARN = lens _kfoRoleARN (\s a -> s {_kfoRoleARN = a})

instance Hashable KinesisFirehoseOutput

instance NFData KinesisFirehoseOutput

instance ToJSON KinesisFirehoseOutput where
  toJSON KinesisFirehoseOutput' {..} =
    object
      ( catMaybes
          [ Just ("ResourceARN" .= _kfoResourceARN),
            Just ("RoleARN" .= _kfoRoleARN)
          ]
      )
