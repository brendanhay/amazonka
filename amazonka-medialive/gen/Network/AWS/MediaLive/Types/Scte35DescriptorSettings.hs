{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35DescriptorSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35DescriptorSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.Scte35SegmentationDescriptor
import qualified Network.AWS.Prelude as Prelude

-- | SCTE-35 Descriptor settings.
--
-- /See:/ 'newScte35DescriptorSettings' smart constructor.
data Scte35DescriptorSettings = Scte35DescriptorSettings'
  { -- | SCTE-35 Segmentation Descriptor.
    segmentationDescriptorScte35DescriptorSettings :: Scte35SegmentationDescriptor
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Scte35DescriptorSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'segmentationDescriptorScte35DescriptorSettings', 'scte35DescriptorSettings_segmentationDescriptorScte35DescriptorSettings' - SCTE-35 Segmentation Descriptor.
newScte35DescriptorSettings ::
  -- | 'segmentationDescriptorScte35DescriptorSettings'
  Scte35SegmentationDescriptor ->
  Scte35DescriptorSettings
newScte35DescriptorSettings
  pSegmentationDescriptorScte35DescriptorSettings_ =
    Scte35DescriptorSettings'
      { segmentationDescriptorScte35DescriptorSettings =
          pSegmentationDescriptorScte35DescriptorSettings_
      }

-- | SCTE-35 Segmentation Descriptor.
scte35DescriptorSettings_segmentationDescriptorScte35DescriptorSettings :: Lens.Lens' Scte35DescriptorSettings Scte35SegmentationDescriptor
scte35DescriptorSettings_segmentationDescriptorScte35DescriptorSettings = Lens.lens (\Scte35DescriptorSettings' {segmentationDescriptorScte35DescriptorSettings} -> segmentationDescriptorScte35DescriptorSettings) (\s@Scte35DescriptorSettings' {} a -> s {segmentationDescriptorScte35DescriptorSettings = a} :: Scte35DescriptorSettings)

instance Prelude.FromJSON Scte35DescriptorSettings where
  parseJSON =
    Prelude.withObject
      "Scte35DescriptorSettings"
      ( \x ->
          Scte35DescriptorSettings'
            Prelude.<$> ( x
                            Prelude..: "segmentationDescriptorScte35DescriptorSettings"
                        )
      )

instance Prelude.Hashable Scte35DescriptorSettings

instance Prelude.NFData Scte35DescriptorSettings

instance Prelude.ToJSON Scte35DescriptorSettings where
  toJSON Scte35DescriptorSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "segmentationDescriptorScte35DescriptorSettings"
                  Prelude..= segmentationDescriptorScte35DescriptorSettings
              )
          ]
      )
