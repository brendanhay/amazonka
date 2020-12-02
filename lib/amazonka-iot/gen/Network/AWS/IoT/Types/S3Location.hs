{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.S3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.S3Location where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The S3 location.
--
--
--
-- /See:/ 's3Location' smart constructor.
data S3Location = S3Location'
  { _slBucket :: !(Maybe Text),
    _slKey :: !(Maybe Text),
    _slVersion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slBucket' - The S3 bucket.
--
-- * 'slKey' - The S3 key.
--
-- * 'slVersion' - The S3 bucket version.
s3Location ::
  S3Location
s3Location =
  S3Location'
    { _slBucket = Nothing,
      _slKey = Nothing,
      _slVersion = Nothing
    }

-- | The S3 bucket.
slBucket :: Lens' S3Location (Maybe Text)
slBucket = lens _slBucket (\s a -> s {_slBucket = a})

-- | The S3 key.
slKey :: Lens' S3Location (Maybe Text)
slKey = lens _slKey (\s a -> s {_slKey = a})

-- | The S3 bucket version.
slVersion :: Lens' S3Location (Maybe Text)
slVersion = lens _slVersion (\s a -> s {_slVersion = a})

instance FromJSON S3Location where
  parseJSON =
    withObject
      "S3Location"
      ( \x ->
          S3Location'
            <$> (x .:? "bucket") <*> (x .:? "key") <*> (x .:? "version")
      )

instance Hashable S3Location

instance NFData S3Location

instance ToJSON S3Location where
  toJSON S3Location' {..} =
    object
      ( catMaybes
          [ ("bucket" .=) <$> _slBucket,
            ("key" .=) <$> _slKey,
            ("version" .=) <$> _slVersion
          ]
      )
