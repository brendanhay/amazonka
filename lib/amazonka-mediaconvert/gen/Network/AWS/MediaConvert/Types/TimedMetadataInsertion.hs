{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TimedMetadataInsertion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TimedMetadataInsertion where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.Id3Insertion
import Network.AWS.Prelude

-- | Enable Timed metadata insertion (TimedMetadataInsertion) to include ID3 tags in any HLS outputs. To include timed metadata, you must enable it here, enable it in each output container, and specify tags and timecodes in ID3 insertion (Id3Insertion) objects.
--
-- /See:/ 'timedMetadataInsertion' smart constructor.
newtype TimedMetadataInsertion = TimedMetadataInsertion'
  { _tmiId3Insertions ::
      Maybe [Id3Insertion]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TimedMetadataInsertion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tmiId3Insertions' - Id3Insertions contains the array of Id3Insertion instances.
timedMetadataInsertion ::
  TimedMetadataInsertion
timedMetadataInsertion =
  TimedMetadataInsertion' {_tmiId3Insertions = Nothing}

-- | Id3Insertions contains the array of Id3Insertion instances.
tmiId3Insertions :: Lens' TimedMetadataInsertion [Id3Insertion]
tmiId3Insertions = lens _tmiId3Insertions (\s a -> s {_tmiId3Insertions = a}) . _Default . _Coerce

instance FromJSON TimedMetadataInsertion where
  parseJSON =
    withObject
      "TimedMetadataInsertion"
      ( \x ->
          TimedMetadataInsertion' <$> (x .:? "id3Insertions" .!= mempty)
      )

instance Hashable TimedMetadataInsertion

instance NFData TimedMetadataInsertion

instance ToJSON TimedMetadataInsertion where
  toJSON TimedMetadataInsertion' {..} =
    object (catMaybes [("id3Insertions" .=) <$> _tmiId3Insertions])
