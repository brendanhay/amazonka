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
-- Module      : Amazonka.SageMaker.Types.FeatureParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.FeatureParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A key-value pair that you specify to describe the feature.
--
-- /See:/ 'newFeatureParameter' smart constructor.
data FeatureParameter = FeatureParameter'
  { -- | A key that must contain a value to describe the feature.
    key :: Prelude.Maybe Prelude.Text,
    -- | The value that belongs to a key.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FeatureParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'featureParameter_key' - A key that must contain a value to describe the feature.
--
-- 'value', 'featureParameter_value' - The value that belongs to a key.
newFeatureParameter ::
  FeatureParameter
newFeatureParameter =
  FeatureParameter'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | A key that must contain a value to describe the feature.
featureParameter_key :: Lens.Lens' FeatureParameter (Prelude.Maybe Prelude.Text)
featureParameter_key = Lens.lens (\FeatureParameter' {key} -> key) (\s@FeatureParameter' {} a -> s {key = a} :: FeatureParameter)

-- | The value that belongs to a key.
featureParameter_value :: Lens.Lens' FeatureParameter (Prelude.Maybe Prelude.Text)
featureParameter_value = Lens.lens (\FeatureParameter' {value} -> value) (\s@FeatureParameter' {} a -> s {value = a} :: FeatureParameter)

instance Data.FromJSON FeatureParameter where
  parseJSON =
    Data.withObject
      "FeatureParameter"
      ( \x ->
          FeatureParameter'
            Prelude.<$> (x Data..:? "Key") Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable FeatureParameter where
  hashWithSalt _salt FeatureParameter' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData FeatureParameter where
  rnf FeatureParameter' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Data.ToJSON FeatureParameter where
  toJSON FeatureParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Key" Data..=) Prelude.<$> key,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
