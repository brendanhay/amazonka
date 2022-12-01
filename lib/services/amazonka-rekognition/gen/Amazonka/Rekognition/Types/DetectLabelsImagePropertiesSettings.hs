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
-- Module      : Amazonka.Rekognition.Types.DetectLabelsImagePropertiesSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.DetectLabelsImagePropertiesSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Settings for the IMAGE_PROPERTIES feature type.
--
-- /See:/ 'newDetectLabelsImagePropertiesSettings' smart constructor.
data DetectLabelsImagePropertiesSettings = DetectLabelsImagePropertiesSettings'
  { -- | The maximum number of dominant colors to return when detecting labels in
    -- an image. The default value is 10.
    maxDominantColors :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectLabelsImagePropertiesSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxDominantColors', 'detectLabelsImagePropertiesSettings_maxDominantColors' - The maximum number of dominant colors to return when detecting labels in
-- an image. The default value is 10.
newDetectLabelsImagePropertiesSettings ::
  DetectLabelsImagePropertiesSettings
newDetectLabelsImagePropertiesSettings =
  DetectLabelsImagePropertiesSettings'
    { maxDominantColors =
        Prelude.Nothing
    }

-- | The maximum number of dominant colors to return when detecting labels in
-- an image. The default value is 10.
detectLabelsImagePropertiesSettings_maxDominantColors :: Lens.Lens' DetectLabelsImagePropertiesSettings (Prelude.Maybe Prelude.Natural)
detectLabelsImagePropertiesSettings_maxDominantColors = Lens.lens (\DetectLabelsImagePropertiesSettings' {maxDominantColors} -> maxDominantColors) (\s@DetectLabelsImagePropertiesSettings' {} a -> s {maxDominantColors = a} :: DetectLabelsImagePropertiesSettings)

instance
  Prelude.Hashable
    DetectLabelsImagePropertiesSettings
  where
  hashWithSalt
    _salt
    DetectLabelsImagePropertiesSettings' {..} =
      _salt `Prelude.hashWithSalt` maxDominantColors

instance
  Prelude.NFData
    DetectLabelsImagePropertiesSettings
  where
  rnf DetectLabelsImagePropertiesSettings' {..} =
    Prelude.rnf maxDominantColors

instance
  Core.ToJSON
    DetectLabelsImagePropertiesSettings
  where
  toJSON DetectLabelsImagePropertiesSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MaxDominantColors" Core..=)
              Prelude.<$> maxDominantColors
          ]
      )
