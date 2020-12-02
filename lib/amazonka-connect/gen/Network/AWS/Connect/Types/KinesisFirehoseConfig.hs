{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.KinesisFirehoseConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.KinesisFirehoseConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration information of a Kinesis Firehose delivery stream.
--
--
--
-- /See:/ 'kinesisFirehoseConfig' smart constructor.
newtype KinesisFirehoseConfig = KinesisFirehoseConfig'
  { _kfcFirehoseARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisFirehoseConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kfcFirehoseARN' - The Amazon Resource Name (ARN) of the delivery stream.
kinesisFirehoseConfig ::
  -- | 'kfcFirehoseARN'
  Text ->
  KinesisFirehoseConfig
kinesisFirehoseConfig pFirehoseARN_ =
  KinesisFirehoseConfig' {_kfcFirehoseARN = pFirehoseARN_}

-- | The Amazon Resource Name (ARN) of the delivery stream.
kfcFirehoseARN :: Lens' KinesisFirehoseConfig Text
kfcFirehoseARN = lens _kfcFirehoseARN (\s a -> s {_kfcFirehoseARN = a})

instance FromJSON KinesisFirehoseConfig where
  parseJSON =
    withObject
      "KinesisFirehoseConfig"
      (\x -> KinesisFirehoseConfig' <$> (x .: "FirehoseArn"))

instance Hashable KinesisFirehoseConfig

instance NFData KinesisFirehoseConfig

instance ToJSON KinesisFirehoseConfig where
  toJSON KinesisFirehoseConfig' {..} =
    object (catMaybes [Just ("FirehoseArn" .= _kfcFirehoseARN)])
