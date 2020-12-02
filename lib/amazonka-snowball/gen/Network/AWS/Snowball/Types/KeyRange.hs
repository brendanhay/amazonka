{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.KeyRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.KeyRange where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains a key range. For export jobs, a @S3Resource@ object can have an optional @KeyRange@ value. The length of the range is defined at job creation, and has either an inclusive @BeginMarker@ , an inclusive @EndMarker@ , or both. Ranges are UTF-8 binary sorted.
--
--
--
-- /See:/ 'keyRange' smart constructor.
data KeyRange = KeyRange'
  { _krEndMarker :: !(Maybe Text),
    _krBeginMarker :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KeyRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'krEndMarker' - The key that ends an optional key range for an export job. Ranges are inclusive and UTF-8 binary sorted.
--
-- * 'krBeginMarker' - The key that starts an optional key range for an export job. Ranges are inclusive and UTF-8 binary sorted.
keyRange ::
  KeyRange
keyRange =
  KeyRange' {_krEndMarker = Nothing, _krBeginMarker = Nothing}

-- | The key that ends an optional key range for an export job. Ranges are inclusive and UTF-8 binary sorted.
krEndMarker :: Lens' KeyRange (Maybe Text)
krEndMarker = lens _krEndMarker (\s a -> s {_krEndMarker = a})

-- | The key that starts an optional key range for an export job. Ranges are inclusive and UTF-8 binary sorted.
krBeginMarker :: Lens' KeyRange (Maybe Text)
krBeginMarker = lens _krBeginMarker (\s a -> s {_krBeginMarker = a})

instance FromJSON KeyRange where
  parseJSON =
    withObject
      "KeyRange"
      ( \x ->
          KeyRange' <$> (x .:? "EndMarker") <*> (x .:? "BeginMarker")
      )

instance Hashable KeyRange

instance NFData KeyRange

instance ToJSON KeyRange where
  toJSON KeyRange' {..} =
    object
      ( catMaybes
          [ ("EndMarker" .=) <$> _krEndMarker,
            ("BeginMarker" .=) <$> _krBeginMarker
          ]
      )
