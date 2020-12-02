{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DestinationSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.S3DestinationSettings
import Network.AWS.Prelude

-- | Settings associated with the destination. Will vary based on the type of destination
--
-- /See:/ 'destinationSettings' smart constructor.
newtype DestinationSettings = DestinationSettings'
  { _dsS3Settings ::
      Maybe S3DestinationSettings
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DestinationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsS3Settings' - Settings associated with S3 destination
destinationSettings ::
  DestinationSettings
destinationSettings = DestinationSettings' {_dsS3Settings = Nothing}

-- | Settings associated with S3 destination
dsS3Settings :: Lens' DestinationSettings (Maybe S3DestinationSettings)
dsS3Settings = lens _dsS3Settings (\s a -> s {_dsS3Settings = a})

instance FromJSON DestinationSettings where
  parseJSON =
    withObject
      "DestinationSettings"
      (\x -> DestinationSettings' <$> (x .:? "s3Settings"))

instance Hashable DestinationSettings

instance NFData DestinationSettings

instance ToJSON DestinationSettings where
  toJSON DestinationSettings' {..} =
    object (catMaybes [("s3Settings" .=) <$> _dsS3Settings])
