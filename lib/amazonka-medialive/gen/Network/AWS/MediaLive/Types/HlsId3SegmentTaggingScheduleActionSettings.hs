{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsId3SegmentTaggingScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsId3SegmentTaggingScheduleActionSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Settings for the action to insert a user-defined ID3 tag in each HLS segment
--
-- /See:/ 'hlsId3SegmentTaggingScheduleActionSettings' smart constructor.
newtype HlsId3SegmentTaggingScheduleActionSettings = HlsId3SegmentTaggingScheduleActionSettings'
  { _histsasTag ::
      Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'HlsId3SegmentTaggingScheduleActionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'histsasTag' - ID3 tag to insert into each segment. Supports special keyword identifiers to substitute in segment-related values.\nSupported keyword identifiers: https://docs.aws.amazon.com/medialive/latest/ug/variable-data-identifiers.html
hlsId3SegmentTaggingScheduleActionSettings ::
  -- | 'histsasTag'
  Text ->
  HlsId3SegmentTaggingScheduleActionSettings
hlsId3SegmentTaggingScheduleActionSettings pTag_ =
  HlsId3SegmentTaggingScheduleActionSettings' {_histsasTag = pTag_}

-- | ID3 tag to insert into each segment. Supports special keyword identifiers to substitute in segment-related values.\nSupported keyword identifiers: https://docs.aws.amazon.com/medialive/latest/ug/variable-data-identifiers.html
histsasTag :: Lens' HlsId3SegmentTaggingScheduleActionSettings Text
histsasTag = lens _histsasTag (\s a -> s {_histsasTag = a})

instance FromJSON HlsId3SegmentTaggingScheduleActionSettings where
  parseJSON =
    withObject
      "HlsId3SegmentTaggingScheduleActionSettings"
      ( \x ->
          HlsId3SegmentTaggingScheduleActionSettings' <$> (x .: "tag")
      )

instance Hashable HlsId3SegmentTaggingScheduleActionSettings

instance NFData HlsId3SegmentTaggingScheduleActionSettings

instance ToJSON HlsId3SegmentTaggingScheduleActionSettings where
  toJSON HlsId3SegmentTaggingScheduleActionSettings' {..} =
    object (catMaybes [Just ("tag" .= _histsasTag)])
