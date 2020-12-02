{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputUpdate where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | When updating an output configuration using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_UpdateApplication.html UpdateApplication> operation, provides information about an Amazon Kinesis Firehose delivery stream configured as the destination.
--
--
--
-- /See:/ 'kinesisFirehoseOutputUpdate' smart constructor.
data KinesisFirehoseOutputUpdate = KinesisFirehoseOutputUpdate'
  { _kfouRoleARNUpdate ::
      !(Maybe Text),
    _kfouResourceARNUpdate ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisFirehoseOutputUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kfouRoleARNUpdate' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
--
-- * 'kfouResourceARNUpdate' - Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery stream to write to.
kinesisFirehoseOutputUpdate ::
  KinesisFirehoseOutputUpdate
kinesisFirehoseOutputUpdate =
  KinesisFirehoseOutputUpdate'
    { _kfouRoleARNUpdate = Nothing,
      _kfouResourceARNUpdate = Nothing
    }

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
kfouRoleARNUpdate :: Lens' KinesisFirehoseOutputUpdate (Maybe Text)
kfouRoleARNUpdate = lens _kfouRoleARNUpdate (\s a -> s {_kfouRoleARNUpdate = a})

-- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery stream to write to.
kfouResourceARNUpdate :: Lens' KinesisFirehoseOutputUpdate (Maybe Text)
kfouResourceARNUpdate = lens _kfouResourceARNUpdate (\s a -> s {_kfouResourceARNUpdate = a})

instance Hashable KinesisFirehoseOutputUpdate

instance NFData KinesisFirehoseOutputUpdate

instance ToJSON KinesisFirehoseOutputUpdate where
  toJSON KinesisFirehoseOutputUpdate' {..} =
    object
      ( catMaybes
          [ ("RoleARNUpdate" .=) <$> _kfouRoleARNUpdate,
            ("ResourceARNUpdate" .=) <$> _kfouResourceARNUpdate
          ]
      )
