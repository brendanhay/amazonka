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
-- Module      : Amazonka.SageMaker.Types.Parameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.Parameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Assigns a value to a named Pipeline parameter.
--
-- /See:/ 'newParameter' smart constructor.
data Parameter = Parameter'
  { -- | The name of the parameter to assign a value to. This parameter name must
    -- match a named parameter in the pipeline definition.
    name :: Prelude.Text,
    -- | The literal value for the parameter.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Parameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'parameter_name' - The name of the parameter to assign a value to. This parameter name must
-- match a named parameter in the pipeline definition.
--
-- 'value', 'parameter_value' - The literal value for the parameter.
newParameter ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  Parameter
newParameter pName_ pValue_ =
  Parameter' {name = pName_, value = pValue_}

-- | The name of the parameter to assign a value to. This parameter name must
-- match a named parameter in the pipeline definition.
parameter_name :: Lens.Lens' Parameter Prelude.Text
parameter_name = Lens.lens (\Parameter' {name} -> name) (\s@Parameter' {} a -> s {name = a} :: Parameter)

-- | The literal value for the parameter.
parameter_value :: Lens.Lens' Parameter Prelude.Text
parameter_value = Lens.lens (\Parameter' {value} -> value) (\s@Parameter' {} a -> s {value = a} :: Parameter)

instance Data.FromJSON Parameter where
  parseJSON =
    Data.withObject
      "Parameter"
      ( \x ->
          Parameter'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable Parameter where
  hashWithSalt _salt Parameter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData Parameter where
  rnf Parameter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON Parameter where
  toJSON Parameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Value" Data..= value)
          ]
      )
