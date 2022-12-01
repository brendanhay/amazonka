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
-- Module      : Amazonka.Rekognition.Types.SegmentTypeInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.SegmentTypeInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.SegmentType

-- | Information about the type of a segment requested in a call to
-- StartSegmentDetection. An array of @SegmentTypeInfo@ objects is returned
-- by the response from GetSegmentDetection.
--
-- /See:/ 'newSegmentTypeInfo' smart constructor.
data SegmentTypeInfo = SegmentTypeInfo'
  { -- | The type of a segment (technical cue or shot detection).
    type' :: Prelude.Maybe SegmentType,
    -- | The version of the model used to detect segments.
    modelVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SegmentTypeInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'segmentTypeInfo_type' - The type of a segment (technical cue or shot detection).
--
-- 'modelVersion', 'segmentTypeInfo_modelVersion' - The version of the model used to detect segments.
newSegmentTypeInfo ::
  SegmentTypeInfo
newSegmentTypeInfo =
  SegmentTypeInfo'
    { type' = Prelude.Nothing,
      modelVersion = Prelude.Nothing
    }

-- | The type of a segment (technical cue or shot detection).
segmentTypeInfo_type :: Lens.Lens' SegmentTypeInfo (Prelude.Maybe SegmentType)
segmentTypeInfo_type = Lens.lens (\SegmentTypeInfo' {type'} -> type') (\s@SegmentTypeInfo' {} a -> s {type' = a} :: SegmentTypeInfo)

-- | The version of the model used to detect segments.
segmentTypeInfo_modelVersion :: Lens.Lens' SegmentTypeInfo (Prelude.Maybe Prelude.Text)
segmentTypeInfo_modelVersion = Lens.lens (\SegmentTypeInfo' {modelVersion} -> modelVersion) (\s@SegmentTypeInfo' {} a -> s {modelVersion = a} :: SegmentTypeInfo)

instance Core.FromJSON SegmentTypeInfo where
  parseJSON =
    Core.withObject
      "SegmentTypeInfo"
      ( \x ->
          SegmentTypeInfo'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "ModelVersion")
      )

instance Prelude.Hashable SegmentTypeInfo where
  hashWithSalt _salt SegmentTypeInfo' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` modelVersion

instance Prelude.NFData SegmentTypeInfo where
  rnf SegmentTypeInfo' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf modelVersion
