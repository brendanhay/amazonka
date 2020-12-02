{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35DescriptorSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35DescriptorSettings where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.Scte35SegmentationDescriptor
import Network.AWS.Prelude

-- | SCTE-35 Descriptor settings.
--
-- /See:/ 'scte35DescriptorSettings' smart constructor.
newtype Scte35DescriptorSettings = Scte35DescriptorSettings'
  { _sdsSegmentationDescriptorScte35DescriptorSettings ::
      Scte35SegmentationDescriptor
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Scte35DescriptorSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdsSegmentationDescriptorScte35DescriptorSettings' - SCTE-35 Segmentation Descriptor.
scte35DescriptorSettings ::
  -- | 'sdsSegmentationDescriptorScte35DescriptorSettings'
  Scte35SegmentationDescriptor ->
  Scte35DescriptorSettings
scte35DescriptorSettings
  pSegmentationDescriptorScte35DescriptorSettings_ =
    Scte35DescriptorSettings'
      { _sdsSegmentationDescriptorScte35DescriptorSettings =
          pSegmentationDescriptorScte35DescriptorSettings_
      }

-- | SCTE-35 Segmentation Descriptor.
sdsSegmentationDescriptorScte35DescriptorSettings :: Lens' Scte35DescriptorSettings Scte35SegmentationDescriptor
sdsSegmentationDescriptorScte35DescriptorSettings = lens _sdsSegmentationDescriptorScte35DescriptorSettings (\s a -> s {_sdsSegmentationDescriptorScte35DescriptorSettings = a})

instance FromJSON Scte35DescriptorSettings where
  parseJSON =
    withObject
      "Scte35DescriptorSettings"
      ( \x ->
          Scte35DescriptorSettings'
            <$> (x .: "segmentationDescriptorScte35DescriptorSettings")
      )

instance Hashable Scte35DescriptorSettings

instance NFData Scte35DescriptorSettings

instance ToJSON Scte35DescriptorSettings where
  toJSON Scte35DescriptorSettings' {..} =
    object
      ( catMaybes
          [ Just
              ( "segmentationDescriptorScte35DescriptorSettings"
                  .= _sdsSegmentationDescriptorScte35DescriptorSettings
              )
          ]
      )
