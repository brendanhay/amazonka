{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.JobAlbumArt
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.JobAlbumArt where

import Network.AWS.ElasticTranscoder.Types.Artwork
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The .jpg or .png file associated with an audio file.
--
--
--
-- /See:/ 'jobAlbumArt' smart constructor.
data JobAlbumArt = JobAlbumArt'
  { _jaaMergePolicy :: !(Maybe Text),
    _jaaArtwork :: !(Maybe [Artwork])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobAlbumArt' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jaaMergePolicy' - A policy that determines how Elastic Transcoder handles the existence of multiple album artwork files.     * @Replace:@ The specified album art replaces any existing album art.     * @Prepend:@ The specified album art is placed in front of any existing album art.     * @Append:@ The specified album art is placed after any existing album art.     * @Fallback:@ If the original input file contains artwork, Elastic Transcoder uses that artwork for the output. If the original input does not contain artwork, Elastic Transcoder uses the specified album art file.
--
-- * 'jaaArtwork' - The file to be used as album art. There can be multiple artworks associated with an audio file, to a maximum of 20. Valid formats are @.jpg@ and @.png@
jobAlbumArt ::
  JobAlbumArt
jobAlbumArt =
  JobAlbumArt' {_jaaMergePolicy = Nothing, _jaaArtwork = Nothing}

-- | A policy that determines how Elastic Transcoder handles the existence of multiple album artwork files.     * @Replace:@ The specified album art replaces any existing album art.     * @Prepend:@ The specified album art is placed in front of any existing album art.     * @Append:@ The specified album art is placed after any existing album art.     * @Fallback:@ If the original input file contains artwork, Elastic Transcoder uses that artwork for the output. If the original input does not contain artwork, Elastic Transcoder uses the specified album art file.
jaaMergePolicy :: Lens' JobAlbumArt (Maybe Text)
jaaMergePolicy = lens _jaaMergePolicy (\s a -> s {_jaaMergePolicy = a})

-- | The file to be used as album art. There can be multiple artworks associated with an audio file, to a maximum of 20. Valid formats are @.jpg@ and @.png@
jaaArtwork :: Lens' JobAlbumArt [Artwork]
jaaArtwork = lens _jaaArtwork (\s a -> s {_jaaArtwork = a}) . _Default . _Coerce

instance FromJSON JobAlbumArt where
  parseJSON =
    withObject
      "JobAlbumArt"
      ( \x ->
          JobAlbumArt'
            <$> (x .:? "MergePolicy") <*> (x .:? "Artwork" .!= mempty)
      )

instance Hashable JobAlbumArt

instance NFData JobAlbumArt

instance ToJSON JobAlbumArt where
  toJSON JobAlbumArt' {..} =
    object
      ( catMaybes
          [ ("MergePolicy" .=) <$> _jaaMergePolicy,
            ("Artwork" .=) <$> _jaaArtwork
          ]
      )
