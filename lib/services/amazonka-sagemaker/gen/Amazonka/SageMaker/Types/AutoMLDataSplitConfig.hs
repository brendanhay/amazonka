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
-- Module      : Amazonka.SageMaker.Types.AutoMLDataSplitConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLDataSplitConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This structure specifies how to split the data into train and validation
-- datasets. The validation and training datasets must contain the same
-- headers. The validation dataset must be less than 2 GB in size.
--
-- /See:/ 'newAutoMLDataSplitConfig' smart constructor.
data AutoMLDataSplitConfig = AutoMLDataSplitConfig'
  { -- | The validation fraction (optional) is a float that specifies the portion
    -- of the training dataset to be used for validation. The default value is
    -- 0.2, and values must be greater than 0 and less than 1. We recommend
    -- setting this value to be less than 0.5.
    validationFraction :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLDataSplitConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validationFraction', 'autoMLDataSplitConfig_validationFraction' - The validation fraction (optional) is a float that specifies the portion
-- of the training dataset to be used for validation. The default value is
-- 0.2, and values must be greater than 0 and less than 1. We recommend
-- setting this value to be less than 0.5.
newAutoMLDataSplitConfig ::
  AutoMLDataSplitConfig
newAutoMLDataSplitConfig =
  AutoMLDataSplitConfig'
    { validationFraction =
        Prelude.Nothing
    }

-- | The validation fraction (optional) is a float that specifies the portion
-- of the training dataset to be used for validation. The default value is
-- 0.2, and values must be greater than 0 and less than 1. We recommend
-- setting this value to be less than 0.5.
autoMLDataSplitConfig_validationFraction :: Lens.Lens' AutoMLDataSplitConfig (Prelude.Maybe Prelude.Double)
autoMLDataSplitConfig_validationFraction = Lens.lens (\AutoMLDataSplitConfig' {validationFraction} -> validationFraction) (\s@AutoMLDataSplitConfig' {} a -> s {validationFraction = a} :: AutoMLDataSplitConfig)

instance Data.FromJSON AutoMLDataSplitConfig where
  parseJSON =
    Data.withObject
      "AutoMLDataSplitConfig"
      ( \x ->
          AutoMLDataSplitConfig'
            Prelude.<$> (x Data..:? "ValidationFraction")
      )

instance Prelude.Hashable AutoMLDataSplitConfig where
  hashWithSalt _salt AutoMLDataSplitConfig' {..} =
    _salt `Prelude.hashWithSalt` validationFraction

instance Prelude.NFData AutoMLDataSplitConfig where
  rnf AutoMLDataSplitConfig' {..} =
    Prelude.rnf validationFraction

instance Data.ToJSON AutoMLDataSplitConfig where
  toJSON AutoMLDataSplitConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ValidationFraction" Data..=)
              Prelude.<$> validationFraction
          ]
      )
