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
-- Module      : Amazonka.SSMIncidents.Types.DynamicSsmParameterValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.DynamicSsmParameterValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMIncidents.Types.VariableType

-- | The dynamic SSM parameter value.
--
-- /See:/ 'newDynamicSsmParameterValue' smart constructor.
data DynamicSsmParameterValue = DynamicSsmParameterValue'
  { -- | Variable dynamic parameters. A parameter value is determined when an
    -- incident is created.
    variable :: Prelude.Maybe VariableType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DynamicSsmParameterValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'variable', 'dynamicSsmParameterValue_variable' - Variable dynamic parameters. A parameter value is determined when an
-- incident is created.
newDynamicSsmParameterValue ::
  DynamicSsmParameterValue
newDynamicSsmParameterValue =
  DynamicSsmParameterValue'
    { variable =
        Prelude.Nothing
    }

-- | Variable dynamic parameters. A parameter value is determined when an
-- incident is created.
dynamicSsmParameterValue_variable :: Lens.Lens' DynamicSsmParameterValue (Prelude.Maybe VariableType)
dynamicSsmParameterValue_variable = Lens.lens (\DynamicSsmParameterValue' {variable} -> variable) (\s@DynamicSsmParameterValue' {} a -> s {variable = a} :: DynamicSsmParameterValue)

instance Data.FromJSON DynamicSsmParameterValue where
  parseJSON =
    Data.withObject
      "DynamicSsmParameterValue"
      ( \x ->
          DynamicSsmParameterValue'
            Prelude.<$> (x Data..:? "variable")
      )

instance Prelude.Hashable DynamicSsmParameterValue where
  hashWithSalt _salt DynamicSsmParameterValue' {..} =
    _salt `Prelude.hashWithSalt` variable

instance Prelude.NFData DynamicSsmParameterValue where
  rnf DynamicSsmParameterValue' {..} =
    Prelude.rnf variable

instance Data.ToJSON DynamicSsmParameterValue where
  toJSON DynamicSsmParameterValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [("variable" Data..=) Prelude.<$> variable]
      )
