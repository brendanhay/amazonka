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
-- Module      : Amazonka.SageMaker.Types.ModelInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Input object for the model.
--
-- /See:/ 'newModelInput' smart constructor.
data ModelInput = ModelInput'
  { -- | The input configuration object for the model.
    dataInputConfig :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataInputConfig', 'modelInput_dataInputConfig' - The input configuration object for the model.
newModelInput ::
  -- | 'dataInputConfig'
  Prelude.Text ->
  ModelInput
newModelInput pDataInputConfig_ =
  ModelInput' {dataInputConfig = pDataInputConfig_}

-- | The input configuration object for the model.
modelInput_dataInputConfig :: Lens.Lens' ModelInput Prelude.Text
modelInput_dataInputConfig = Lens.lens (\ModelInput' {dataInputConfig} -> dataInputConfig) (\s@ModelInput' {} a -> s {dataInputConfig = a} :: ModelInput)

instance Data.FromJSON ModelInput where
  parseJSON =
    Data.withObject
      "ModelInput"
      ( \x ->
          ModelInput'
            Prelude.<$> (x Data..: "DataInputConfig")
      )

instance Prelude.Hashable ModelInput where
  hashWithSalt _salt ModelInput' {..} =
    _salt `Prelude.hashWithSalt` dataInputConfig

instance Prelude.NFData ModelInput where
  rnf ModelInput' {..} = Prelude.rnf dataInputConfig

instance Data.ToJSON ModelInput where
  toJSON ModelInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DataInputConfig" Data..= dataInputConfig)
          ]
      )
