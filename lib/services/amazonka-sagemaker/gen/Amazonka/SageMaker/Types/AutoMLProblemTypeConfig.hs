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
-- Module      : Amazonka.SageMaker.Types.AutoMLProblemTypeConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLProblemTypeConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ImageClassificationJobConfig
import Amazonka.SageMaker.Types.TabularJobConfig
import Amazonka.SageMaker.Types.TextClassificationJobConfig

-- | A collection of settings specific to the problem type used to configure
-- an AutoML job V2. There must be one and only one config of the following
-- type.
--
-- /See:/ 'newAutoMLProblemTypeConfig' smart constructor.
data AutoMLProblemTypeConfig = AutoMLProblemTypeConfig'
  { -- | Settings used to configure an AutoML job V2 for the image classification
    -- problem type.
    imageClassificationJobConfig :: Prelude.Maybe ImageClassificationJobConfig,
    -- | Settings used to configure an AutoML job V2 for a tabular problem type
    -- (regression, classification).
    tabularJobConfig :: Prelude.Maybe TabularJobConfig,
    -- | Settings used to configure an AutoML job V2 for the text classification
    -- problem type.
    textClassificationJobConfig :: Prelude.Maybe TextClassificationJobConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLProblemTypeConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageClassificationJobConfig', 'autoMLProblemTypeConfig_imageClassificationJobConfig' - Settings used to configure an AutoML job V2 for the image classification
-- problem type.
--
-- 'tabularJobConfig', 'autoMLProblemTypeConfig_tabularJobConfig' - Settings used to configure an AutoML job V2 for a tabular problem type
-- (regression, classification).
--
-- 'textClassificationJobConfig', 'autoMLProblemTypeConfig_textClassificationJobConfig' - Settings used to configure an AutoML job V2 for the text classification
-- problem type.
newAutoMLProblemTypeConfig ::
  AutoMLProblemTypeConfig
newAutoMLProblemTypeConfig =
  AutoMLProblemTypeConfig'
    { imageClassificationJobConfig =
        Prelude.Nothing,
      tabularJobConfig = Prelude.Nothing,
      textClassificationJobConfig = Prelude.Nothing
    }

-- | Settings used to configure an AutoML job V2 for the image classification
-- problem type.
autoMLProblemTypeConfig_imageClassificationJobConfig :: Lens.Lens' AutoMLProblemTypeConfig (Prelude.Maybe ImageClassificationJobConfig)
autoMLProblemTypeConfig_imageClassificationJobConfig = Lens.lens (\AutoMLProblemTypeConfig' {imageClassificationJobConfig} -> imageClassificationJobConfig) (\s@AutoMLProblemTypeConfig' {} a -> s {imageClassificationJobConfig = a} :: AutoMLProblemTypeConfig)

-- | Settings used to configure an AutoML job V2 for a tabular problem type
-- (regression, classification).
autoMLProblemTypeConfig_tabularJobConfig :: Lens.Lens' AutoMLProblemTypeConfig (Prelude.Maybe TabularJobConfig)
autoMLProblemTypeConfig_tabularJobConfig = Lens.lens (\AutoMLProblemTypeConfig' {tabularJobConfig} -> tabularJobConfig) (\s@AutoMLProblemTypeConfig' {} a -> s {tabularJobConfig = a} :: AutoMLProblemTypeConfig)

-- | Settings used to configure an AutoML job V2 for the text classification
-- problem type.
autoMLProblemTypeConfig_textClassificationJobConfig :: Lens.Lens' AutoMLProblemTypeConfig (Prelude.Maybe TextClassificationJobConfig)
autoMLProblemTypeConfig_textClassificationJobConfig = Lens.lens (\AutoMLProblemTypeConfig' {textClassificationJobConfig} -> textClassificationJobConfig) (\s@AutoMLProblemTypeConfig' {} a -> s {textClassificationJobConfig = a} :: AutoMLProblemTypeConfig)

instance Data.FromJSON AutoMLProblemTypeConfig where
  parseJSON =
    Data.withObject
      "AutoMLProblemTypeConfig"
      ( \x ->
          AutoMLProblemTypeConfig'
            Prelude.<$> (x Data..:? "ImageClassificationJobConfig")
            Prelude.<*> (x Data..:? "TabularJobConfig")
            Prelude.<*> (x Data..:? "TextClassificationJobConfig")
      )

instance Prelude.Hashable AutoMLProblemTypeConfig where
  hashWithSalt _salt AutoMLProblemTypeConfig' {..} =
    _salt
      `Prelude.hashWithSalt` imageClassificationJobConfig
      `Prelude.hashWithSalt` tabularJobConfig
      `Prelude.hashWithSalt` textClassificationJobConfig

instance Prelude.NFData AutoMLProblemTypeConfig where
  rnf AutoMLProblemTypeConfig' {..} =
    Prelude.rnf imageClassificationJobConfig
      `Prelude.seq` Prelude.rnf tabularJobConfig
      `Prelude.seq` Prelude.rnf textClassificationJobConfig

instance Data.ToJSON AutoMLProblemTypeConfig where
  toJSON AutoMLProblemTypeConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ImageClassificationJobConfig" Data..=)
              Prelude.<$> imageClassificationJobConfig,
            ("TabularJobConfig" Data..=)
              Prelude.<$> tabularJobConfig,
            ("TextClassificationJobConfig" Data..=)
              Prelude.<$> textClassificationJobConfig
          ]
      )
