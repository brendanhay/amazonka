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
-- Module      : Amazonka.SageMaker.Types.EnvironmentParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.EnvironmentParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of environment parameters suggested by the Amazon SageMaker
-- Inference Recommender.
--
-- /See:/ 'newEnvironmentParameter' smart constructor.
data EnvironmentParameter = EnvironmentParameter'
  { -- | The environment key suggested by the Amazon SageMaker Inference
    -- Recommender.
    key :: Prelude.Text,
    -- | The value type suggested by the Amazon SageMaker Inference Recommender.
    valueType :: Prelude.Text,
    -- | The value suggested by the Amazon SageMaker Inference Recommender.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'environmentParameter_key' - The environment key suggested by the Amazon SageMaker Inference
-- Recommender.
--
-- 'valueType', 'environmentParameter_valueType' - The value type suggested by the Amazon SageMaker Inference Recommender.
--
-- 'value', 'environmentParameter_value' - The value suggested by the Amazon SageMaker Inference Recommender.
newEnvironmentParameter ::
  -- | 'key'
  Prelude.Text ->
  -- | 'valueType'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  EnvironmentParameter
newEnvironmentParameter pKey_ pValueType_ pValue_ =
  EnvironmentParameter'
    { key = pKey_,
      valueType = pValueType_,
      value = pValue_
    }

-- | The environment key suggested by the Amazon SageMaker Inference
-- Recommender.
environmentParameter_key :: Lens.Lens' EnvironmentParameter Prelude.Text
environmentParameter_key = Lens.lens (\EnvironmentParameter' {key} -> key) (\s@EnvironmentParameter' {} a -> s {key = a} :: EnvironmentParameter)

-- | The value type suggested by the Amazon SageMaker Inference Recommender.
environmentParameter_valueType :: Lens.Lens' EnvironmentParameter Prelude.Text
environmentParameter_valueType = Lens.lens (\EnvironmentParameter' {valueType} -> valueType) (\s@EnvironmentParameter' {} a -> s {valueType = a} :: EnvironmentParameter)

-- | The value suggested by the Amazon SageMaker Inference Recommender.
environmentParameter_value :: Lens.Lens' EnvironmentParameter Prelude.Text
environmentParameter_value = Lens.lens (\EnvironmentParameter' {value} -> value) (\s@EnvironmentParameter' {} a -> s {value = a} :: EnvironmentParameter)

instance Data.FromJSON EnvironmentParameter where
  parseJSON =
    Data.withObject
      "EnvironmentParameter"
      ( \x ->
          EnvironmentParameter'
            Prelude.<$> (x Data..: "Key")
            Prelude.<*> (x Data..: "ValueType")
            Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable EnvironmentParameter where
  hashWithSalt _salt EnvironmentParameter' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` valueType
      `Prelude.hashWithSalt` value

instance Prelude.NFData EnvironmentParameter where
  rnf EnvironmentParameter' {..} =
    Prelude.rnf key `Prelude.seq`
      Prelude.rnf valueType `Prelude.seq`
        Prelude.rnf value
