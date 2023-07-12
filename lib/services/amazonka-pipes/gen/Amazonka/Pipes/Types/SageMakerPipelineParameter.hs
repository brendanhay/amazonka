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
-- Module      : Amazonka.Pipes.Types.SageMakerPipelineParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.SageMakerPipelineParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Name\/Value pair of a parameter to start execution of a SageMaker Model
-- Building Pipeline.
--
-- /See:/ 'newSageMakerPipelineParameter' smart constructor.
data SageMakerPipelineParameter = SageMakerPipelineParameter'
  { -- | Name of parameter to start execution of a SageMaker Model Building
    -- Pipeline.
    name :: Data.Sensitive Prelude.Text,
    -- | Value of parameter to start execution of a SageMaker Model Building
    -- Pipeline.
    value :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SageMakerPipelineParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'sageMakerPipelineParameter_name' - Name of parameter to start execution of a SageMaker Model Building
-- Pipeline.
--
-- 'value', 'sageMakerPipelineParameter_value' - Value of parameter to start execution of a SageMaker Model Building
-- Pipeline.
newSageMakerPipelineParameter ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  SageMakerPipelineParameter
newSageMakerPipelineParameter pName_ pValue_ =
  SageMakerPipelineParameter'
    { name =
        Data._Sensitive Lens.# pName_,
      value = Data._Sensitive Lens.# pValue_
    }

-- | Name of parameter to start execution of a SageMaker Model Building
-- Pipeline.
sageMakerPipelineParameter_name :: Lens.Lens' SageMakerPipelineParameter Prelude.Text
sageMakerPipelineParameter_name = Lens.lens (\SageMakerPipelineParameter' {name} -> name) (\s@SageMakerPipelineParameter' {} a -> s {name = a} :: SageMakerPipelineParameter) Prelude.. Data._Sensitive

-- | Value of parameter to start execution of a SageMaker Model Building
-- Pipeline.
sageMakerPipelineParameter_value :: Lens.Lens' SageMakerPipelineParameter Prelude.Text
sageMakerPipelineParameter_value = Lens.lens (\SageMakerPipelineParameter' {value} -> value) (\s@SageMakerPipelineParameter' {} a -> s {value = a} :: SageMakerPipelineParameter) Prelude.. Data._Sensitive

instance Data.FromJSON SageMakerPipelineParameter where
  parseJSON =
    Data.withObject
      "SageMakerPipelineParameter"
      ( \x ->
          SageMakerPipelineParameter'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable SageMakerPipelineParameter where
  hashWithSalt _salt SageMakerPipelineParameter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData SageMakerPipelineParameter where
  rnf SageMakerPipelineParameter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON SageMakerPipelineParameter where
  toJSON SageMakerPipelineParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Value" Data..= value)
          ]
      )
