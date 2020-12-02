{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutput where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | When configuring application output, identifies an Amazon Kinesis stream as the destination. You provide the stream Amazon Resource Name (ARN) and also an IAM role ARN that Amazon Kinesis Analytics can use to write to the stream on your behalf.
--
--
--
-- /See:/ 'kinesisStreamsOutput' smart constructor.
data KinesisStreamsOutput = KinesisStreamsOutput'
  { _ksoResourceARN ::
      !Text,
    _ksoRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisStreamsOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ksoResourceARN' - ARN of the destination Amazon Kinesis stream to write to.
--
-- * 'ksoRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination stream on your behalf. You need to grant the necessary permissions to this role.
kinesisStreamsOutput ::
  -- | 'ksoResourceARN'
  Text ->
  -- | 'ksoRoleARN'
  Text ->
  KinesisStreamsOutput
kinesisStreamsOutput pResourceARN_ pRoleARN_ =
  KinesisStreamsOutput'
    { _ksoResourceARN = pResourceARN_,
      _ksoRoleARN = pRoleARN_
    }

-- | ARN of the destination Amazon Kinesis stream to write to.
ksoResourceARN :: Lens' KinesisStreamsOutput Text
ksoResourceARN = lens _ksoResourceARN (\s a -> s {_ksoResourceARN = a})

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination stream on your behalf. You need to grant the necessary permissions to this role.
ksoRoleARN :: Lens' KinesisStreamsOutput Text
ksoRoleARN = lens _ksoRoleARN (\s a -> s {_ksoRoleARN = a})

instance Hashable KinesisStreamsOutput

instance NFData KinesisStreamsOutput

instance ToJSON KinesisStreamsOutput where
  toJSON KinesisStreamsOutput' {..} =
    object
      ( catMaybes
          [ Just ("ResourceARN" .= _ksoResourceARN),
            Just ("RoleARN" .= _ksoRoleARN)
          ]
      )
