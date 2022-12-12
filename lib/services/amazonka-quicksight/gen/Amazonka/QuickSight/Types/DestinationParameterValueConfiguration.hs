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
-- Module      : Amazonka.QuickSight.Types.DestinationParameterValueConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DestinationParameterValueConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CustomValuesConfiguration
import Amazonka.QuickSight.Types.SelectAllValueOptions

-- | The configuration of destination parameter values.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newDestinationParameterValueConfiguration' smart constructor.
data DestinationParameterValueConfiguration = DestinationParameterValueConfiguration'
  { -- | The configuration of custom values for destination parameter in
    -- @DestinationParameterValueConfiguration@.
    customValuesConfiguration :: Prelude.Maybe CustomValuesConfiguration,
    -- | The configuration that selects all options.
    selectAllValueOptions :: Prelude.Maybe SelectAllValueOptions,
    -- | The source field ID of the destination parameter.
    sourceField :: Prelude.Maybe Prelude.Text,
    -- | The source parameter name of the destination parameter.
    sourceParameterName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DestinationParameterValueConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customValuesConfiguration', 'destinationParameterValueConfiguration_customValuesConfiguration' - The configuration of custom values for destination parameter in
-- @DestinationParameterValueConfiguration@.
--
-- 'selectAllValueOptions', 'destinationParameterValueConfiguration_selectAllValueOptions' - The configuration that selects all options.
--
-- 'sourceField', 'destinationParameterValueConfiguration_sourceField' - The source field ID of the destination parameter.
--
-- 'sourceParameterName', 'destinationParameterValueConfiguration_sourceParameterName' - The source parameter name of the destination parameter.
newDestinationParameterValueConfiguration ::
  DestinationParameterValueConfiguration
newDestinationParameterValueConfiguration =
  DestinationParameterValueConfiguration'
    { customValuesConfiguration =
        Prelude.Nothing,
      selectAllValueOptions =
        Prelude.Nothing,
      sourceField = Prelude.Nothing,
      sourceParameterName =
        Prelude.Nothing
    }

-- | The configuration of custom values for destination parameter in
-- @DestinationParameterValueConfiguration@.
destinationParameterValueConfiguration_customValuesConfiguration :: Lens.Lens' DestinationParameterValueConfiguration (Prelude.Maybe CustomValuesConfiguration)
destinationParameterValueConfiguration_customValuesConfiguration = Lens.lens (\DestinationParameterValueConfiguration' {customValuesConfiguration} -> customValuesConfiguration) (\s@DestinationParameterValueConfiguration' {} a -> s {customValuesConfiguration = a} :: DestinationParameterValueConfiguration)

-- | The configuration that selects all options.
destinationParameterValueConfiguration_selectAllValueOptions :: Lens.Lens' DestinationParameterValueConfiguration (Prelude.Maybe SelectAllValueOptions)
destinationParameterValueConfiguration_selectAllValueOptions = Lens.lens (\DestinationParameterValueConfiguration' {selectAllValueOptions} -> selectAllValueOptions) (\s@DestinationParameterValueConfiguration' {} a -> s {selectAllValueOptions = a} :: DestinationParameterValueConfiguration)

-- | The source field ID of the destination parameter.
destinationParameterValueConfiguration_sourceField :: Lens.Lens' DestinationParameterValueConfiguration (Prelude.Maybe Prelude.Text)
destinationParameterValueConfiguration_sourceField = Lens.lens (\DestinationParameterValueConfiguration' {sourceField} -> sourceField) (\s@DestinationParameterValueConfiguration' {} a -> s {sourceField = a} :: DestinationParameterValueConfiguration)

-- | The source parameter name of the destination parameter.
destinationParameterValueConfiguration_sourceParameterName :: Lens.Lens' DestinationParameterValueConfiguration (Prelude.Maybe Prelude.Text)
destinationParameterValueConfiguration_sourceParameterName = Lens.lens (\DestinationParameterValueConfiguration' {sourceParameterName} -> sourceParameterName) (\s@DestinationParameterValueConfiguration' {} a -> s {sourceParameterName = a} :: DestinationParameterValueConfiguration)

instance
  Data.FromJSON
    DestinationParameterValueConfiguration
  where
  parseJSON =
    Data.withObject
      "DestinationParameterValueConfiguration"
      ( \x ->
          DestinationParameterValueConfiguration'
            Prelude.<$> (x Data..:? "CustomValuesConfiguration")
            Prelude.<*> (x Data..:? "SelectAllValueOptions")
            Prelude.<*> (x Data..:? "SourceField")
            Prelude.<*> (x Data..:? "SourceParameterName")
      )

instance
  Prelude.Hashable
    DestinationParameterValueConfiguration
  where
  hashWithSalt
    _salt
    DestinationParameterValueConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` customValuesConfiguration
        `Prelude.hashWithSalt` selectAllValueOptions
        `Prelude.hashWithSalt` sourceField
        `Prelude.hashWithSalt` sourceParameterName

instance
  Prelude.NFData
    DestinationParameterValueConfiguration
  where
  rnf DestinationParameterValueConfiguration' {..} =
    Prelude.rnf customValuesConfiguration
      `Prelude.seq` Prelude.rnf selectAllValueOptions
      `Prelude.seq` Prelude.rnf sourceField
      `Prelude.seq` Prelude.rnf sourceParameterName

instance
  Data.ToJSON
    DestinationParameterValueConfiguration
  where
  toJSON DestinationParameterValueConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomValuesConfiguration" Data..=)
              Prelude.<$> customValuesConfiguration,
            ("SelectAllValueOptions" Data..=)
              Prelude.<$> selectAllValueOptions,
            ("SourceField" Data..=) Prelude.<$> sourceField,
            ("SourceParameterName" Data..=)
              Prelude.<$> sourceParameterName
          ]
      )
