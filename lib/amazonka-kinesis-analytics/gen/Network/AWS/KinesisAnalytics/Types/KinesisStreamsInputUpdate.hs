{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputUpdate where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | When updating application input configuration, provides information about an Amazon Kinesis stream as the streaming source.
--
--
--
-- /See:/ 'kinesisStreamsInputUpdate' smart constructor.
data KinesisStreamsInputUpdate = KinesisStreamsInputUpdate'
  { _ksiuRoleARNUpdate ::
      !(Maybe Text),
    _ksiuResourceARNUpdate :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisStreamsInputUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ksiuRoleARNUpdate' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
--
-- * 'ksiuResourceARNUpdate' - Amazon Resource Name (ARN) of the input Amazon Kinesis stream to read.
kinesisStreamsInputUpdate ::
  KinesisStreamsInputUpdate
kinesisStreamsInputUpdate =
  KinesisStreamsInputUpdate'
    { _ksiuRoleARNUpdate = Nothing,
      _ksiuResourceARNUpdate = Nothing
    }

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
ksiuRoleARNUpdate :: Lens' KinesisStreamsInputUpdate (Maybe Text)
ksiuRoleARNUpdate = lens _ksiuRoleARNUpdate (\s a -> s {_ksiuRoleARNUpdate = a})

-- | Amazon Resource Name (ARN) of the input Amazon Kinesis stream to read.
ksiuResourceARNUpdate :: Lens' KinesisStreamsInputUpdate (Maybe Text)
ksiuResourceARNUpdate = lens _ksiuResourceARNUpdate (\s a -> s {_ksiuResourceARNUpdate = a})

instance Hashable KinesisStreamsInputUpdate

instance NFData KinesisStreamsInputUpdate

instance ToJSON KinesisStreamsInputUpdate where
  toJSON KinesisStreamsInputUpdate' {..} =
    object
      ( catMaybes
          [ ("RoleARNUpdate" .=) <$> _ksiuRoleARNUpdate,
            ("ResourceARNUpdate" .=) <$> _ksiuResourceARNUpdate
          ]
      )
