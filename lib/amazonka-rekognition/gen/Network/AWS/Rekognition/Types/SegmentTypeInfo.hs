{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.SegmentTypeInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.SegmentTypeInfo where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.SegmentType

-- | Information about the type of a segment requested in a call to 'StartSegmentDetection' . An array of @SegmentTypeInfo@ objects is returned by the response from 'GetSegmentDetection' .
--
--
--
-- /See:/ 'segmentTypeInfo' smart constructor.
data SegmentTypeInfo = SegmentTypeInfo'
  { _stiModelVersion ::
      !(Maybe Text),
    _stiType :: !(Maybe SegmentType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SegmentTypeInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stiModelVersion' - The version of the model used to detect segments.
--
-- * 'stiType' - The type of a segment (technical cue or shot detection).
segmentTypeInfo ::
  SegmentTypeInfo
segmentTypeInfo =
  SegmentTypeInfo' {_stiModelVersion = Nothing, _stiType = Nothing}

-- | The version of the model used to detect segments.
stiModelVersion :: Lens' SegmentTypeInfo (Maybe Text)
stiModelVersion = lens _stiModelVersion (\s a -> s {_stiModelVersion = a})

-- | The type of a segment (technical cue or shot detection).
stiType :: Lens' SegmentTypeInfo (Maybe SegmentType)
stiType = lens _stiType (\s a -> s {_stiType = a})

instance FromJSON SegmentTypeInfo where
  parseJSON =
    withObject
      "SegmentTypeInfo"
      ( \x ->
          SegmentTypeInfo' <$> (x .:? "ModelVersion") <*> (x .:? "Type")
      )

instance Hashable SegmentTypeInfo

instance NFData SegmentTypeInfo
