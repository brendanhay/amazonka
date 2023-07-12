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
-- Module      : Amazonka.Forecast.Types.AttributeConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.AttributeConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the method used to transform attributes.
--
-- The following is an example using the RETAIL domain:
--
-- @{@
--
-- @\"AttributeName\": \"demand\",@
--
-- @\"Transformations\": {\"aggregation\": \"sum\", \"middlefill\": \"zero\", \"backfill\": \"zero\"}@
--
-- @}@
--
-- /See:/ 'newAttributeConfig' smart constructor.
data AttributeConfig = AttributeConfig'
  { -- | The name of the attribute as specified in the schema. Amazon Forecast
    -- supports the target field of the target time series and the related time
    -- series datasets. For example, for the RETAIL domain, the target is
    -- @demand@.
    attributeName :: Prelude.Text,
    -- | The method parameters (key-value pairs), which are a map of override
    -- parameters. Specify these parameters to override the default values.
    -- Related Time Series attributes do not accept aggregation parameters.
    --
    -- The following list shows the parameters and their valid values for the
    -- \"filling\" featurization method for a __Target Time Series__ dataset.
    -- Default values are bolded.
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
    transformations :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttributeConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeName', 'attributeConfig_attributeName' - The name of the attribute as specified in the schema. Amazon Forecast
-- supports the target field of the target time series and the related time
-- series datasets. For example, for the RETAIL domain, the target is
-- @demand@.
--
-- 'transformations', 'attributeConfig_transformations' - The method parameters (key-value pairs), which are a map of override
-- parameters. Specify these parameters to override the default values.
-- Related Time Series attributes do not accept aggregation parameters.
--
-- The following list shows the parameters and their valid values for the
-- \"filling\" featurization method for a __Target Time Series__ dataset.
-- Default values are bolded.
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
newAttributeConfig ::
  -- | 'attributeName'
  Prelude.Text ->
  AttributeConfig
newAttributeConfig pAttributeName_ =
  AttributeConfig'
    { attributeName = pAttributeName_,
      transformations = Prelude.mempty
    }

-- | The name of the attribute as specified in the schema. Amazon Forecast
-- supports the target field of the target time series and the related time
-- series datasets. For example, for the RETAIL domain, the target is
-- @demand@.
attributeConfig_attributeName :: Lens.Lens' AttributeConfig Prelude.Text
attributeConfig_attributeName = Lens.lens (\AttributeConfig' {attributeName} -> attributeName) (\s@AttributeConfig' {} a -> s {attributeName = a} :: AttributeConfig)

-- | The method parameters (key-value pairs), which are a map of override
-- parameters. Specify these parameters to override the default values.
-- Related Time Series attributes do not accept aggregation parameters.
--
-- The following list shows the parameters and their valid values for the
-- \"filling\" featurization method for a __Target Time Series__ dataset.
-- Default values are bolded.
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
attributeConfig_transformations :: Lens.Lens' AttributeConfig (Prelude.HashMap Prelude.Text Prelude.Text)
attributeConfig_transformations = Lens.lens (\AttributeConfig' {transformations} -> transformations) (\s@AttributeConfig' {} a -> s {transformations = a} :: AttributeConfig) Prelude.. Lens.coerced

instance Data.FromJSON AttributeConfig where
  parseJSON =
    Data.withObject
      "AttributeConfig"
      ( \x ->
          AttributeConfig'
            Prelude.<$> (x Data..: "AttributeName")
            Prelude.<*> ( x
                            Data..:? "Transformations"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AttributeConfig where
  hashWithSalt _salt AttributeConfig' {..} =
    _salt
      `Prelude.hashWithSalt` attributeName
      `Prelude.hashWithSalt` transformations

instance Prelude.NFData AttributeConfig where
  rnf AttributeConfig' {..} =
    Prelude.rnf attributeName
      `Prelude.seq` Prelude.rnf transformations

instance Data.ToJSON AttributeConfig where
  toJSON AttributeConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AttributeName" Data..= attributeName),
            Prelude.Just
              ("Transformations" Data..= transformations)
          ]
      )
