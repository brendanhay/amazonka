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
-- Module      : Network.AWS.Rekognition.Types.SegmentTypeInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.SegmentTypeInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.SegmentType

-- | Information about the type of a segment requested in a call to
-- StartSegmentDetection. An array of @SegmentTypeInfo@ objects is returned
-- by the response from GetSegmentDetection.
--
-- /See:/ 'newSegmentTypeInfo' smart constructor.
data SegmentTypeInfo = SegmentTypeInfo'
  { -- | The version of the model used to detect segments.
    modelVersion :: Prelude.Maybe Prelude.Text,
    -- | The type of a segment (technical cue or shot detection).
    type' :: Prelude.Maybe SegmentType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SegmentTypeInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelVersion', 'segmentTypeInfo_modelVersion' - The version of the model used to detect segments.
--
-- 'type'', 'segmentTypeInfo_type' - The type of a segment (technical cue or shot detection).
newSegmentTypeInfo ::
  SegmentTypeInfo
newSegmentTypeInfo =
  SegmentTypeInfo'
    { modelVersion = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The version of the model used to detect segments.
segmentTypeInfo_modelVersion :: Lens.Lens' SegmentTypeInfo (Prelude.Maybe Prelude.Text)
segmentTypeInfo_modelVersion = Lens.lens (\SegmentTypeInfo' {modelVersion} -> modelVersion) (\s@SegmentTypeInfo' {} a -> s {modelVersion = a} :: SegmentTypeInfo)

-- | The type of a segment (technical cue or shot detection).
segmentTypeInfo_type :: Lens.Lens' SegmentTypeInfo (Prelude.Maybe SegmentType)
segmentTypeInfo_type = Lens.lens (\SegmentTypeInfo' {type'} -> type') (\s@SegmentTypeInfo' {} a -> s {type' = a} :: SegmentTypeInfo)

instance Prelude.FromJSON SegmentTypeInfo where
  parseJSON =
    Prelude.withObject
      "SegmentTypeInfo"
      ( \x ->
          SegmentTypeInfo'
            Prelude.<$> (x Prelude..:? "ModelVersion")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable SegmentTypeInfo

instance Prelude.NFData SegmentTypeInfo
