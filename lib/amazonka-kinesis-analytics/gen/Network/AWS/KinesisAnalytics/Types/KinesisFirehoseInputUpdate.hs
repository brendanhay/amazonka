{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputUpdate where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | When updating application input configuration, provides information about an Amazon Kinesis Firehose delivery stream as the streaming source.
--
--
--
-- /See:/ 'kinesisFirehoseInputUpdate' smart constructor.
data KinesisFirehoseInputUpdate = KinesisFirehoseInputUpdate'
  { _kfiuRoleARNUpdate ::
      !(Maybe Text),
    _kfiuResourceARNUpdate ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisFirehoseInputUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kfiuRoleARNUpdate' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
--
-- * 'kfiuResourceARNUpdate' - Amazon Resource Name (ARN) of the input Amazon Kinesis Firehose delivery stream to read.
kinesisFirehoseInputUpdate ::
  KinesisFirehoseInputUpdate
kinesisFirehoseInputUpdate =
  KinesisFirehoseInputUpdate'
    { _kfiuRoleARNUpdate = Nothing,
      _kfiuResourceARNUpdate = Nothing
    }

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
kfiuRoleARNUpdate :: Lens' KinesisFirehoseInputUpdate (Maybe Text)
kfiuRoleARNUpdate = lens _kfiuRoleARNUpdate (\s a -> s {_kfiuRoleARNUpdate = a})

-- | Amazon Resource Name (ARN) of the input Amazon Kinesis Firehose delivery stream to read.
kfiuResourceARNUpdate :: Lens' KinesisFirehoseInputUpdate (Maybe Text)
kfiuResourceARNUpdate = lens _kfiuResourceARNUpdate (\s a -> s {_kfiuResourceARNUpdate = a})

instance Hashable KinesisFirehoseInputUpdate

instance NFData KinesisFirehoseInputUpdate

instance ToJSON KinesisFirehoseInputUpdate where
  toJSON KinesisFirehoseInputUpdate' {..} =
    object
      ( catMaybes
          [ ("RoleARNUpdate" .=) <$> _kfiuRoleARNUpdate,
            ("ResourceARNUpdate" .=) <$> _kfiuResourceARNUpdate
          ]
      )
