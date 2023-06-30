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
-- Module      : Amazonka.Rekognition.Types.DetectLabelsSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.DetectLabelsSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.DetectLabelsImagePropertiesSettings
import Amazonka.Rekognition.Types.GeneralLabelsSettings

-- | Settings for the DetectLabels request. Settings can include filters for
-- both GENERAL_LABELS and IMAGE_PROPERTIES. GENERAL_LABELS filters can be
-- inclusive or exclusive and applied to individual labels or label
-- categories. IMAGE_PROPERTIES filters allow specification of a maximum
-- number of dominant colors.
--
-- /See:/ 'newDetectLabelsSettings' smart constructor.
data DetectLabelsSettings = DetectLabelsSettings'
  { -- | Contains the specified filters for GENERAL_LABELS.
    generalLabels :: Prelude.Maybe GeneralLabelsSettings,
    -- | Contains the chosen number of maximum dominant colors in an image.
    imageProperties :: Prelude.Maybe DetectLabelsImagePropertiesSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectLabelsSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'generalLabels', 'detectLabelsSettings_generalLabels' - Contains the specified filters for GENERAL_LABELS.
--
-- 'imageProperties', 'detectLabelsSettings_imageProperties' - Contains the chosen number of maximum dominant colors in an image.
newDetectLabelsSettings ::
  DetectLabelsSettings
newDetectLabelsSettings =
  DetectLabelsSettings'
    { generalLabels =
        Prelude.Nothing,
      imageProperties = Prelude.Nothing
    }

-- | Contains the specified filters for GENERAL_LABELS.
detectLabelsSettings_generalLabels :: Lens.Lens' DetectLabelsSettings (Prelude.Maybe GeneralLabelsSettings)
detectLabelsSettings_generalLabels = Lens.lens (\DetectLabelsSettings' {generalLabels} -> generalLabels) (\s@DetectLabelsSettings' {} a -> s {generalLabels = a} :: DetectLabelsSettings)

-- | Contains the chosen number of maximum dominant colors in an image.
detectLabelsSettings_imageProperties :: Lens.Lens' DetectLabelsSettings (Prelude.Maybe DetectLabelsImagePropertiesSettings)
detectLabelsSettings_imageProperties = Lens.lens (\DetectLabelsSettings' {imageProperties} -> imageProperties) (\s@DetectLabelsSettings' {} a -> s {imageProperties = a} :: DetectLabelsSettings)

instance Prelude.Hashable DetectLabelsSettings where
  hashWithSalt _salt DetectLabelsSettings' {..} =
    _salt
      `Prelude.hashWithSalt` generalLabels
      `Prelude.hashWithSalt` imageProperties

instance Prelude.NFData DetectLabelsSettings where
  rnf DetectLabelsSettings' {..} =
    Prelude.rnf generalLabels
      `Prelude.seq` Prelude.rnf imageProperties

instance Data.ToJSON DetectLabelsSettings where
  toJSON DetectLabelsSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GeneralLabels" Data..=) Prelude.<$> generalLabels,
            ("ImageProperties" Data..=)
              Prelude.<$> imageProperties
          ]
      )
