{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.FileLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.FileLocation where

import Network.AWS.IoT.Types.S3Location
import Network.AWS.IoT.Types.Stream
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The location of the OTA update.
--
--
--
-- /See:/ 'fileLocation' smart constructor.
data FileLocation = FileLocation'
  { _flStream :: !(Maybe Stream),
    _flS3Location :: !(Maybe S3Location)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FileLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'flStream' - The stream that contains the OTA update.
--
-- * 'flS3Location' - The location of the updated firmware in S3.
fileLocation ::
  FileLocation
fileLocation =
  FileLocation' {_flStream = Nothing, _flS3Location = Nothing}

-- | The stream that contains the OTA update.
flStream :: Lens' FileLocation (Maybe Stream)
flStream = lens _flStream (\s a -> s {_flStream = a})

-- | The location of the updated firmware in S3.
flS3Location :: Lens' FileLocation (Maybe S3Location)
flS3Location = lens _flS3Location (\s a -> s {_flS3Location = a})

instance FromJSON FileLocation where
  parseJSON =
    withObject
      "FileLocation"
      ( \x ->
          FileLocation' <$> (x .:? "stream") <*> (x .:? "s3Location")
      )

instance Hashable FileLocation

instance NFData FileLocation

instance ToJSON FileLocation where
  toJSON FileLocation' {..} =
    object
      ( catMaybes
          [("stream" .=) <$> _flStream, ("s3Location" .=) <$> _flS3Location]
      )
