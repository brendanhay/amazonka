{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Id3Insertion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Id3Insertion where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | To insert ID3 tags in your output, specify two values. Use ID3 tag (Id3) to specify the base 64 encoded string and use Timecode (TimeCode) to specify the time when the tag should be inserted. To insert multiple ID3 tags in your output, create multiple instances of ID3 insertion (Id3Insertion).
--
-- /See:/ 'id3Insertion' smart constructor.
data Id3Insertion = Id3Insertion'
  { _iiId3 :: !(Maybe Text),
    _iiTimecode :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Id3Insertion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iiId3' - Use ID3 tag (Id3) to provide a tag value in base64-encode format.
--
-- * 'iiTimecode' - Provide a Timecode (TimeCode) in HH:MM:SS:FF or HH:MM:SS;FF format.
id3Insertion ::
  Id3Insertion
id3Insertion =
  Id3Insertion' {_iiId3 = Nothing, _iiTimecode = Nothing}

-- | Use ID3 tag (Id3) to provide a tag value in base64-encode format.
iiId3 :: Lens' Id3Insertion (Maybe Text)
iiId3 = lens _iiId3 (\s a -> s {_iiId3 = a})

-- | Provide a Timecode (TimeCode) in HH:MM:SS:FF or HH:MM:SS;FF format.
iiTimecode :: Lens' Id3Insertion (Maybe Text)
iiTimecode = lens _iiTimecode (\s a -> s {_iiTimecode = a})

instance FromJSON Id3Insertion where
  parseJSON =
    withObject
      "Id3Insertion"
      (\x -> Id3Insertion' <$> (x .:? "id3") <*> (x .:? "timecode"))

instance Hashable Id3Insertion

instance NFData Id3Insertion

instance ToJSON Id3Insertion where
  toJSON Id3Insertion' {..} =
    object
      ( catMaybes
          [("id3" .=) <$> _iiId3, ("timecode" .=) <$> _iiTimecode]
      )
