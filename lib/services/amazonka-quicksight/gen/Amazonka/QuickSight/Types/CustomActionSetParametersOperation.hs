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
-- Module      : Amazonka.QuickSight.Types.CustomActionSetParametersOperation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CustomActionSetParametersOperation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.SetParameterValueConfiguration

-- | The set parameter operation that sets parameters in custom action.
--
-- /See:/ 'newCustomActionSetParametersOperation' smart constructor.
data CustomActionSetParametersOperation = CustomActionSetParametersOperation'
  { -- | The parameter that determines the value configuration.
    parameterValueConfigurations :: Prelude.NonEmpty SetParameterValueConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomActionSetParametersOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterValueConfigurations', 'customActionSetParametersOperation_parameterValueConfigurations' - The parameter that determines the value configuration.
newCustomActionSetParametersOperation ::
  -- | 'parameterValueConfigurations'
  Prelude.NonEmpty SetParameterValueConfiguration ->
  CustomActionSetParametersOperation
newCustomActionSetParametersOperation
  pParameterValueConfigurations_ =
    CustomActionSetParametersOperation'
      { parameterValueConfigurations =
          Lens.coerced
            Lens.# pParameterValueConfigurations_
      }

-- | The parameter that determines the value configuration.
customActionSetParametersOperation_parameterValueConfigurations :: Lens.Lens' CustomActionSetParametersOperation (Prelude.NonEmpty SetParameterValueConfiguration)
customActionSetParametersOperation_parameterValueConfigurations = Lens.lens (\CustomActionSetParametersOperation' {parameterValueConfigurations} -> parameterValueConfigurations) (\s@CustomActionSetParametersOperation' {} a -> s {parameterValueConfigurations = a} :: CustomActionSetParametersOperation) Prelude.. Lens.coerced

instance
  Data.FromJSON
    CustomActionSetParametersOperation
  where
  parseJSON =
    Data.withObject
      "CustomActionSetParametersOperation"
      ( \x ->
          CustomActionSetParametersOperation'
            Prelude.<$> (x Data..: "ParameterValueConfigurations")
      )

instance
  Prelude.Hashable
    CustomActionSetParametersOperation
  where
  hashWithSalt
    _salt
    CustomActionSetParametersOperation' {..} =
      _salt
        `Prelude.hashWithSalt` parameterValueConfigurations

instance
  Prelude.NFData
    CustomActionSetParametersOperation
  where
  rnf CustomActionSetParametersOperation' {..} =
    Prelude.rnf parameterValueConfigurations

instance
  Data.ToJSON
    CustomActionSetParametersOperation
  where
  toJSON CustomActionSetParametersOperation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ParameterValueConfigurations"
                  Data..= parameterValueConfigurations
              )
          ]
      )
