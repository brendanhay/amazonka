{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.KinesisVideoStreamConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.KinesisVideoStreamConfig where

import Network.AWS.Connect.Types.EncryptionConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration information of a Kinesis video stream.
--
--
--
-- /See:/ 'kinesisVideoStreamConfig' smart constructor.
data KinesisVideoStreamConfig = KinesisVideoStreamConfig'
  { _kvscPrefix ::
      !Text,
    _kvscRetentionPeriodHours :: !Nat,
    _kvscEncryptionConfig ::
      !EncryptionConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KinesisVideoStreamConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kvscPrefix' - The prefix of the video stream.
--
-- * 'kvscRetentionPeriodHours' - The number of hours data is retained in the stream. Kinesis Video Streams retains the data in a data store that is associated with the stream. The default value is 0, indicating that the stream does not persist data.
--
-- * 'kvscEncryptionConfig' - The encryption configuration.
kinesisVideoStreamConfig ::
  -- | 'kvscPrefix'
  Text ->
  -- | 'kvscRetentionPeriodHours'
  Natural ->
  -- | 'kvscEncryptionConfig'
  EncryptionConfig ->
  KinesisVideoStreamConfig
kinesisVideoStreamConfig
  pPrefix_
  pRetentionPeriodHours_
  pEncryptionConfig_ =
    KinesisVideoStreamConfig'
      { _kvscPrefix = pPrefix_,
        _kvscRetentionPeriodHours = _Nat # pRetentionPeriodHours_,
        _kvscEncryptionConfig = pEncryptionConfig_
      }

-- | The prefix of the video stream.
kvscPrefix :: Lens' KinesisVideoStreamConfig Text
kvscPrefix = lens _kvscPrefix (\s a -> s {_kvscPrefix = a})

-- | The number of hours data is retained in the stream. Kinesis Video Streams retains the data in a data store that is associated with the stream. The default value is 0, indicating that the stream does not persist data.
kvscRetentionPeriodHours :: Lens' KinesisVideoStreamConfig Natural
kvscRetentionPeriodHours = lens _kvscRetentionPeriodHours (\s a -> s {_kvscRetentionPeriodHours = a}) . _Nat

-- | The encryption configuration.
kvscEncryptionConfig :: Lens' KinesisVideoStreamConfig EncryptionConfig
kvscEncryptionConfig = lens _kvscEncryptionConfig (\s a -> s {_kvscEncryptionConfig = a})

instance FromJSON KinesisVideoStreamConfig where
  parseJSON =
    withObject
      "KinesisVideoStreamConfig"
      ( \x ->
          KinesisVideoStreamConfig'
            <$> (x .: "Prefix")
            <*> (x .: "RetentionPeriodHours")
            <*> (x .: "EncryptionConfig")
      )

instance Hashable KinesisVideoStreamConfig

instance NFData KinesisVideoStreamConfig

instance ToJSON KinesisVideoStreamConfig where
  toJSON KinesisVideoStreamConfig' {..} =
    object
      ( catMaybes
          [ Just ("Prefix" .= _kvscPrefix),
            Just ("RetentionPeriodHours" .= _kvscRetentionPeriodHours),
            Just ("EncryptionConfig" .= _kvscEncryptionConfig)
          ]
      )
