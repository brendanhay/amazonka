{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisStreamsInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisStreamsInput where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Identifies an Amazon Kinesis stream as the streaming source. You provide the stream's Amazon Resource Name (ARN) and an IAM role ARN that enables Amazon Kinesis Analytics to access the stream on your behalf.
--
--
--
-- /See:/ 'kinesisStreamsInput' smart constructor.
data KinesisStreamsInput = KinesisStreamsInput'
  { _ksiResourceARN ::
      !Text,
    _ksiRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisStreamsInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ksiResourceARN' - ARN of the input Amazon Kinesis stream to read.
--
-- * 'ksiRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
kinesisStreamsInput ::
  -- | 'ksiResourceARN'
  Text ->
  -- | 'ksiRoleARN'
  Text ->
  KinesisStreamsInput
kinesisStreamsInput pResourceARN_ pRoleARN_ =
  KinesisStreamsInput'
    { _ksiResourceARN = pResourceARN_,
      _ksiRoleARN = pRoleARN_
    }

-- | ARN of the input Amazon Kinesis stream to read.
ksiResourceARN :: Lens' KinesisStreamsInput Text
ksiResourceARN = lens _ksiResourceARN (\s a -> s {_ksiResourceARN = a})

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
ksiRoleARN :: Lens' KinesisStreamsInput Text
ksiRoleARN = lens _ksiRoleARN (\s a -> s {_ksiRoleARN = a})

instance Hashable KinesisStreamsInput

instance NFData KinesisStreamsInput

instance ToJSON KinesisStreamsInput where
  toJSON KinesisStreamsInput' {..} =
    object
      ( catMaybes
          [ Just ("ResourceARN" .= _ksiResourceARN),
            Just ("RoleARN" .= _ksiRoleARN)
          ]
      )
