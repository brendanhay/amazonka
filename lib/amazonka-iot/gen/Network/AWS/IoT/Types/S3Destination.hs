{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.S3Destination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.S3Destination where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the location of updated firmware in S3.
--
--
--
-- /See:/ 's3Destination' smart constructor.
data S3Destination = S3Destination'
  { _sdPrefix :: !(Maybe Text),
    _sdBucket :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3Destination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdPrefix' - The S3 prefix.
--
-- * 'sdBucket' - The S3 bucket that contains the updated firmware.
s3Destination ::
  S3Destination
s3Destination =
  S3Destination' {_sdPrefix = Nothing, _sdBucket = Nothing}

-- | The S3 prefix.
sdPrefix :: Lens' S3Destination (Maybe Text)
sdPrefix = lens _sdPrefix (\s a -> s {_sdPrefix = a})

-- | The S3 bucket that contains the updated firmware.
sdBucket :: Lens' S3Destination (Maybe Text)
sdBucket = lens _sdBucket (\s a -> s {_sdBucket = a})

instance FromJSON S3Destination where
  parseJSON =
    withObject
      "S3Destination"
      (\x -> S3Destination' <$> (x .:? "prefix") <*> (x .:? "bucket"))

instance Hashable S3Destination

instance NFData S3Destination

instance ToJSON S3Destination where
  toJSON S3Destination' {..} =
    object
      ( catMaybes
          [("prefix" .=) <$> _sdPrefix, ("bucket" .=) <$> _sdBucket]
      )
