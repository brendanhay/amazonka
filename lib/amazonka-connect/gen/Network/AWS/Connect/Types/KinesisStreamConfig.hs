{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.KinesisStreamConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.KinesisStreamConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration information of a Kinesis data stream.
--
--
--
-- /See:/ 'kinesisStreamConfig' smart constructor.
newtype KinesisStreamConfig = KinesisStreamConfig'
  { _kscStreamARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisStreamConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kscStreamARN' - The Amazon Resource Name (ARN) of the data stream.
kinesisStreamConfig ::
  -- | 'kscStreamARN'
  Text ->
  KinesisStreamConfig
kinesisStreamConfig pStreamARN_ =
  KinesisStreamConfig' {_kscStreamARN = pStreamARN_}

-- | The Amazon Resource Name (ARN) of the data stream.
kscStreamARN :: Lens' KinesisStreamConfig Text
kscStreamARN = lens _kscStreamARN (\s a -> s {_kscStreamARN = a})

instance FromJSON KinesisStreamConfig where
  parseJSON =
    withObject
      "KinesisStreamConfig"
      (\x -> KinesisStreamConfig' <$> (x .: "StreamArn"))

instance Hashable KinesisStreamConfig

instance NFData KinesisStreamConfig

instance ToJSON KinesisStreamConfig where
  toJSON KinesisStreamConfig' {..} =
    object (catMaybes [Just ("StreamArn" .= _kscStreamARN)])
