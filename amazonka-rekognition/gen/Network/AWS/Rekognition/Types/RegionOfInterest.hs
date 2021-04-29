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
-- Module      : Network.AWS.Rekognition.Types.RegionOfInterest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.RegionOfInterest where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.BoundingBox

-- | Specifies a location within the frame that Rekognition checks for text.
-- Uses a @BoundingBox@ object to set a region of the screen.
--
-- A word is included in the region if the word is more than half in that
-- region. If there is more than one region, the word will be compared with
-- all regions of the screen. Any word more than half in a region is kept
-- in the results.
--
-- /See:/ 'newRegionOfInterest' smart constructor.
data RegionOfInterest = RegionOfInterest'
  { -- | The box representing a region of interest on screen.
    boundingBox :: Prelude.Maybe BoundingBox
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegionOfInterest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'boundingBox', 'regionOfInterest_boundingBox' - The box representing a region of interest on screen.
newRegionOfInterest ::
  RegionOfInterest
newRegionOfInterest =
  RegionOfInterest' {boundingBox = Prelude.Nothing}

-- | The box representing a region of interest on screen.
regionOfInterest_boundingBox :: Lens.Lens' RegionOfInterest (Prelude.Maybe BoundingBox)
regionOfInterest_boundingBox = Lens.lens (\RegionOfInterest' {boundingBox} -> boundingBox) (\s@RegionOfInterest' {} a -> s {boundingBox = a} :: RegionOfInterest)

instance Prelude.Hashable RegionOfInterest

instance Prelude.NFData RegionOfInterest

instance Prelude.ToJSON RegionOfInterest where
  toJSON RegionOfInterest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("BoundingBox" Prelude..=) Prelude.<$> boundingBox]
      )
