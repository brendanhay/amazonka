{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentDimensions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentDimensions where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.AttributeDimension
import Network.AWS.Pinpoint.Types.MetricDimension
import Network.AWS.Pinpoint.Types.SegmentBehaviors
import Network.AWS.Pinpoint.Types.SegmentDemographics
import Network.AWS.Pinpoint.Types.SegmentLocation
import Network.AWS.Prelude

-- | Specifies the dimension settings for a segment.
--
--
--
-- /See:/ 'segmentDimensions' smart constructor.
data SegmentDimensions = SegmentDimensions'
  { _sdMetrics ::
      !(Maybe (Map Text (MetricDimension))),
    _sdLocation :: !(Maybe SegmentLocation),
    _sdDemographic :: !(Maybe SegmentDemographics),
    _sdUserAttributes ::
      !(Maybe (Map Text (AttributeDimension))),
    _sdBehavior :: !(Maybe SegmentBehaviors),
    _sdAttributes ::
      !(Maybe (Map Text (AttributeDimension)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SegmentDimensions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdMetrics' - One or more custom metrics to use as criteria for the segment.
--
-- * 'sdLocation' - The location-based criteria, such as region or GPS coordinates, for the segment.
--
-- * 'sdDemographic' - The demographic-based criteria, such as device platform, for the segment.
--
-- * 'sdUserAttributes' - One or more custom user attributes to use as criteria for the segment.
--
-- * 'sdBehavior' - The behavior-based criteria, such as how recently users have used your app, for the segment.
--
-- * 'sdAttributes' - One or more custom attributes to use as criteria for the segment.
segmentDimensions ::
  SegmentDimensions
segmentDimensions =
  SegmentDimensions'
    { _sdMetrics = Nothing,
      _sdLocation = Nothing,
      _sdDemographic = Nothing,
      _sdUserAttributes = Nothing,
      _sdBehavior = Nothing,
      _sdAttributes = Nothing
    }

-- | One or more custom metrics to use as criteria for the segment.
sdMetrics :: Lens' SegmentDimensions (HashMap Text (MetricDimension))
sdMetrics = lens _sdMetrics (\s a -> s {_sdMetrics = a}) . _Default . _Map

-- | The location-based criteria, such as region or GPS coordinates, for the segment.
sdLocation :: Lens' SegmentDimensions (Maybe SegmentLocation)
sdLocation = lens _sdLocation (\s a -> s {_sdLocation = a})

-- | The demographic-based criteria, such as device platform, for the segment.
sdDemographic :: Lens' SegmentDimensions (Maybe SegmentDemographics)
sdDemographic = lens _sdDemographic (\s a -> s {_sdDemographic = a})

-- | One or more custom user attributes to use as criteria for the segment.
sdUserAttributes :: Lens' SegmentDimensions (HashMap Text (AttributeDimension))
sdUserAttributes = lens _sdUserAttributes (\s a -> s {_sdUserAttributes = a}) . _Default . _Map

-- | The behavior-based criteria, such as how recently users have used your app, for the segment.
sdBehavior :: Lens' SegmentDimensions (Maybe SegmentBehaviors)
sdBehavior = lens _sdBehavior (\s a -> s {_sdBehavior = a})

-- | One or more custom attributes to use as criteria for the segment.
sdAttributes :: Lens' SegmentDimensions (HashMap Text (AttributeDimension))
sdAttributes = lens _sdAttributes (\s a -> s {_sdAttributes = a}) . _Default . _Map

instance FromJSON SegmentDimensions where
  parseJSON =
    withObject
      "SegmentDimensions"
      ( \x ->
          SegmentDimensions'
            <$> (x .:? "Metrics" .!= mempty)
            <*> (x .:? "Location")
            <*> (x .:? "Demographic")
            <*> (x .:? "UserAttributes" .!= mempty)
            <*> (x .:? "Behavior")
            <*> (x .:? "Attributes" .!= mempty)
      )

instance Hashable SegmentDimensions

instance NFData SegmentDimensions

instance ToJSON SegmentDimensions where
  toJSON SegmentDimensions' {..} =
    object
      ( catMaybes
          [ ("Metrics" .=) <$> _sdMetrics,
            ("Location" .=) <$> _sdLocation,
            ("Demographic" .=) <$> _sdDemographic,
            ("UserAttributes" .=) <$> _sdUserAttributes,
            ("Behavior" .=) <$> _sdBehavior,
            ("Attributes" .=) <$> _sdAttributes
          ]
      )
