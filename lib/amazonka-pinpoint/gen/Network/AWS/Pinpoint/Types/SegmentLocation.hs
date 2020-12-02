{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentLocation where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.GPSPointDimension
import Network.AWS.Pinpoint.Types.SetDimension
import Network.AWS.Prelude

-- | Specifies geographical dimension settings for a segment.
--
--
--
-- /See:/ 'segmentLocation' smart constructor.
data SegmentLocation = SegmentLocation'
  { _slCountry ::
      !(Maybe SetDimension),
    _slGPSPoint :: !(Maybe GPSPointDimension)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SegmentLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slCountry' - The country or region code, in ISO 3166-1 alpha-2 format, for the segment.
--
-- * 'slGPSPoint' - The GPS location and range for the segment.
segmentLocation ::
  SegmentLocation
segmentLocation =
  SegmentLocation' {_slCountry = Nothing, _slGPSPoint = Nothing}

-- | The country or region code, in ISO 3166-1 alpha-2 format, for the segment.
slCountry :: Lens' SegmentLocation (Maybe SetDimension)
slCountry = lens _slCountry (\s a -> s {_slCountry = a})

-- | The GPS location and range for the segment.
slGPSPoint :: Lens' SegmentLocation (Maybe GPSPointDimension)
slGPSPoint = lens _slGPSPoint (\s a -> s {_slGPSPoint = a})

instance FromJSON SegmentLocation where
  parseJSON =
    withObject
      "SegmentLocation"
      ( \x ->
          SegmentLocation' <$> (x .:? "Country") <*> (x .:? "GPSPoint")
      )

instance Hashable SegmentLocation

instance NFData SegmentLocation

instance ToJSON SegmentLocation where
  toJSON SegmentLocation' {..} =
    object
      ( catMaybes
          [("Country" .=) <$> _slCountry, ("GPSPoint" .=) <$> _slGPSPoint]
      )
