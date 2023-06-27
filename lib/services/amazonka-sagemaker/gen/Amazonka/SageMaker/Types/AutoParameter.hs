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
-- Module      : Amazonka.SageMaker.Types.AutoParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The name and an example value of the hyperparameter that you want to use
-- in Autotune. If Automatic model tuning (AMT) determines that your
-- hyperparameter is eligible for Autotune, an optimal hyperparameter range
-- is selected for you.
--
-- /See:/ 'newAutoParameter' smart constructor.
data AutoParameter = AutoParameter'
  { -- | The name of the hyperparameter to optimize using Autotune.
    name :: Prelude.Text,
    -- | An example value of the hyperparameter to optimize using Autotune.
    valueHint :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'autoParameter_name' - The name of the hyperparameter to optimize using Autotune.
--
-- 'valueHint', 'autoParameter_valueHint' - An example value of the hyperparameter to optimize using Autotune.
newAutoParameter ::
  -- | 'name'
  Prelude.Text ->
  -- | 'valueHint'
  Prelude.Text ->
  AutoParameter
newAutoParameter pName_ pValueHint_ =
  AutoParameter'
    { name = pName_,
      valueHint = pValueHint_
    }

-- | The name of the hyperparameter to optimize using Autotune.
autoParameter_name :: Lens.Lens' AutoParameter Prelude.Text
autoParameter_name = Lens.lens (\AutoParameter' {name} -> name) (\s@AutoParameter' {} a -> s {name = a} :: AutoParameter)

-- | An example value of the hyperparameter to optimize using Autotune.
autoParameter_valueHint :: Lens.Lens' AutoParameter Prelude.Text
autoParameter_valueHint = Lens.lens (\AutoParameter' {valueHint} -> valueHint) (\s@AutoParameter' {} a -> s {valueHint = a} :: AutoParameter)

instance Data.FromJSON AutoParameter where
  parseJSON =
    Data.withObject
      "AutoParameter"
      ( \x ->
          AutoParameter'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "ValueHint")
      )

instance Prelude.Hashable AutoParameter where
  hashWithSalt _salt AutoParameter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` valueHint

instance Prelude.NFData AutoParameter where
  rnf AutoParameter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf valueHint

instance Data.ToJSON AutoParameter where
  toJSON AutoParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("ValueHint" Data..= valueHint)
          ]
      )
