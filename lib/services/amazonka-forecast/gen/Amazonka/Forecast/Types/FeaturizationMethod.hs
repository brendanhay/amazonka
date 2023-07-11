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
-- Module      : Amazonka.Forecast.Types.FeaturizationMethod
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.FeaturizationMethod where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.FeaturizationMethodName
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the method that featurizes (transforms) a
-- dataset field. The method is part of the @FeaturizationPipeline@ of the
-- Featurization object.
--
-- The following is an example of how you specify a @FeaturizationMethod@
-- object.
--
-- @{@
--
-- @\"FeaturizationMethodName\": \"filling\",@
--
-- @\"FeaturizationMethodParameters\": {\"aggregation\": \"sum\", \"middlefill\": \"zero\", \"backfill\": \"zero\"}@
--
-- @}@
--
-- /See:/ 'newFeaturizationMethod' smart constructor.
data FeaturizationMethod = FeaturizationMethod'
  { -- | The method parameters (key-value pairs), which are a map of override
    -- parameters. Specify these parameters to override the default values.
    -- Related Time Series attributes do not accept aggregation parameters.
    --
    -- The following list shows the parameters and their valid values for the
    -- \"filling\" featurization method for a __Target Time Series__ dataset.
    -- Bold signifies the default value.
    --
    -- -   @aggregation@: __sum__, @avg@, @first@, @min@, @max@
    --
    -- -   @frontfill@: __none__
    --
    -- -   @middlefill@: __zero__, @nan@ (not a number), @value@, @median@,
    --     @mean@, @min@, @max@
    --
    -- -   @backfill@: __zero__, @nan@, @value@, @median@, @mean@, @min@, @max@
    --
    -- The following list shows the parameters and their valid values for a
    -- __Related Time Series__ featurization method (there are no defaults):
    --
    -- -   @middlefill@: @zero@, @value@, @median@, @mean@, @min@, @max@
    --
    -- -   @backfill@: @zero@, @value@, @median@, @mean@, @min@, @max@
    --
    -- -   @futurefill@: @zero@, @value@, @median@, @mean@, @min@, @max@
    --
    -- To set a filling method to a specific value, set the fill parameter to
    -- @value@ and define the value in a corresponding @_value@ parameter. For
    -- example, to set backfilling to a value of 2, include the following:
    -- @\"backfill\": \"value\"@ and @\"backfill_value\":\"2\"@.
    featurizationMethodParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the method. The \"filling\" method is the only supported
    -- method.
    featurizationMethodName :: FeaturizationMethodName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FeaturizationMethod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featurizationMethodParameters', 'featurizationMethod_featurizationMethodParameters' - The method parameters (key-value pairs), which are a map of override
-- parameters. Specify these parameters to override the default values.
-- Related Time Series attributes do not accept aggregation parameters.
--
-- The following list shows the parameters and their valid values for the
-- \"filling\" featurization method for a __Target Time Series__ dataset.
-- Bold signifies the default value.
--
-- -   @aggregation@: __sum__, @avg@, @first@, @min@, @max@
--
-- -   @frontfill@: __none__
--
-- -   @middlefill@: __zero__, @nan@ (not a number), @value@, @median@,
--     @mean@, @min@, @max@
--
-- -   @backfill@: __zero__, @nan@, @value@, @median@, @mean@, @min@, @max@
--
-- The following list shows the parameters and their valid values for a
-- __Related Time Series__ featurization method (there are no defaults):
--
-- -   @middlefill@: @zero@, @value@, @median@, @mean@, @min@, @max@
--
-- -   @backfill@: @zero@, @value@, @median@, @mean@, @min@, @max@
--
-- -   @futurefill@: @zero@, @value@, @median@, @mean@, @min@, @max@
--
-- To set a filling method to a specific value, set the fill parameter to
-- @value@ and define the value in a corresponding @_value@ parameter. For
-- example, to set backfilling to a value of 2, include the following:
-- @\"backfill\": \"value\"@ and @\"backfill_value\":\"2\"@.
--
-- 'featurizationMethodName', 'featurizationMethod_featurizationMethodName' - The name of the method. The \"filling\" method is the only supported
-- method.
newFeaturizationMethod ::
  -- | 'featurizationMethodName'
  FeaturizationMethodName ->
  FeaturizationMethod
newFeaturizationMethod pFeaturizationMethodName_ =
  FeaturizationMethod'
    { featurizationMethodParameters =
        Prelude.Nothing,
      featurizationMethodName = pFeaturizationMethodName_
    }

-- | The method parameters (key-value pairs), which are a map of override
-- parameters. Specify these parameters to override the default values.
-- Related Time Series attributes do not accept aggregation parameters.
--
-- The following list shows the parameters and their valid values for the
-- \"filling\" featurization method for a __Target Time Series__ dataset.
-- Bold signifies the default value.
--
-- -   @aggregation@: __sum__, @avg@, @first@, @min@, @max@
--
-- -   @frontfill@: __none__
--
-- -   @middlefill@: __zero__, @nan@ (not a number), @value@, @median@,
--     @mean@, @min@, @max@
--
-- -   @backfill@: __zero__, @nan@, @value@, @median@, @mean@, @min@, @max@
--
-- The following list shows the parameters and their valid values for a
-- __Related Time Series__ featurization method (there are no defaults):
--
-- -   @middlefill@: @zero@, @value@, @median@, @mean@, @min@, @max@
--
-- -   @backfill@: @zero@, @value@, @median@, @mean@, @min@, @max@
--
-- -   @futurefill@: @zero@, @value@, @median@, @mean@, @min@, @max@
--
-- To set a filling method to a specific value, set the fill parameter to
-- @value@ and define the value in a corresponding @_value@ parameter. For
-- example, to set backfilling to a value of 2, include the following:
-- @\"backfill\": \"value\"@ and @\"backfill_value\":\"2\"@.
featurizationMethod_featurizationMethodParameters :: Lens.Lens' FeaturizationMethod (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
featurizationMethod_featurizationMethodParameters = Lens.lens (\FeaturizationMethod' {featurizationMethodParameters} -> featurizationMethodParameters) (\s@FeaturizationMethod' {} a -> s {featurizationMethodParameters = a} :: FeaturizationMethod) Prelude.. Lens.mapping Lens.coerced

-- | The name of the method. The \"filling\" method is the only supported
-- method.
featurizationMethod_featurizationMethodName :: Lens.Lens' FeaturizationMethod FeaturizationMethodName
featurizationMethod_featurizationMethodName = Lens.lens (\FeaturizationMethod' {featurizationMethodName} -> featurizationMethodName) (\s@FeaturizationMethod' {} a -> s {featurizationMethodName = a} :: FeaturizationMethod)

instance Data.FromJSON FeaturizationMethod where
  parseJSON =
    Data.withObject
      "FeaturizationMethod"
      ( \x ->
          FeaturizationMethod'
            Prelude.<$> ( x
                            Data..:? "FeaturizationMethodParameters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "FeaturizationMethodName")
      )

instance Prelude.Hashable FeaturizationMethod where
  hashWithSalt _salt FeaturizationMethod' {..} =
    _salt
      `Prelude.hashWithSalt` featurizationMethodParameters
      `Prelude.hashWithSalt` featurizationMethodName

instance Prelude.NFData FeaturizationMethod where
  rnf FeaturizationMethod' {..} =
    Prelude.rnf featurizationMethodParameters
      `Prelude.seq` Prelude.rnf featurizationMethodName

instance Data.ToJSON FeaturizationMethod where
  toJSON FeaturizationMethod' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FeaturizationMethodParameters" Data..=)
              Prelude.<$> featurizationMethodParameters,
            Prelude.Just
              ( "FeaturizationMethodName"
                  Data..= featurizationMethodName
              )
          ]
      )
