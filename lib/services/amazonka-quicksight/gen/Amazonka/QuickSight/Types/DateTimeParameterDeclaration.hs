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
-- Module      : Amazonka.QuickSight.Types.DateTimeParameterDeclaration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DateTimeParameterDeclaration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DateTimeDefaultValues
import Amazonka.QuickSight.Types.DateTimeValueWhenUnsetConfiguration
import Amazonka.QuickSight.Types.TimeGranularity

-- | A parameter declaration for the @DateTime@ data type.
--
-- /See:/ 'newDateTimeParameterDeclaration' smart constructor.
data DateTimeParameterDeclaration = DateTimeParameterDeclaration'
  { -- | The default values of a parameter. If the parameter is a single-value
    -- parameter, a maximum of one default value can be provided.
    defaultValues :: Prelude.Maybe DateTimeDefaultValues,
    -- | The level of time precision that is used to aggregate @DateTime@ values.
    timeGranularity :: Prelude.Maybe TimeGranularity,
    -- | The configuration that defines the default value of a @DateTime@
    -- parameter when a value has not been set.
    valueWhenUnset :: Prelude.Maybe DateTimeValueWhenUnsetConfiguration,
    -- | The name of the parameter that is being declared.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DateTimeParameterDeclaration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValues', 'dateTimeParameterDeclaration_defaultValues' - The default values of a parameter. If the parameter is a single-value
-- parameter, a maximum of one default value can be provided.
--
-- 'timeGranularity', 'dateTimeParameterDeclaration_timeGranularity' - The level of time precision that is used to aggregate @DateTime@ values.
--
-- 'valueWhenUnset', 'dateTimeParameterDeclaration_valueWhenUnset' - The configuration that defines the default value of a @DateTime@
-- parameter when a value has not been set.
--
-- 'name', 'dateTimeParameterDeclaration_name' - The name of the parameter that is being declared.
newDateTimeParameterDeclaration ::
  -- | 'name'
  Prelude.Text ->
  DateTimeParameterDeclaration
newDateTimeParameterDeclaration pName_ =
  DateTimeParameterDeclaration'
    { defaultValues =
        Prelude.Nothing,
      timeGranularity = Prelude.Nothing,
      valueWhenUnset = Prelude.Nothing,
      name = pName_
    }

-- | The default values of a parameter. If the parameter is a single-value
-- parameter, a maximum of one default value can be provided.
dateTimeParameterDeclaration_defaultValues :: Lens.Lens' DateTimeParameterDeclaration (Prelude.Maybe DateTimeDefaultValues)
dateTimeParameterDeclaration_defaultValues = Lens.lens (\DateTimeParameterDeclaration' {defaultValues} -> defaultValues) (\s@DateTimeParameterDeclaration' {} a -> s {defaultValues = a} :: DateTimeParameterDeclaration)

-- | The level of time precision that is used to aggregate @DateTime@ values.
dateTimeParameterDeclaration_timeGranularity :: Lens.Lens' DateTimeParameterDeclaration (Prelude.Maybe TimeGranularity)
dateTimeParameterDeclaration_timeGranularity = Lens.lens (\DateTimeParameterDeclaration' {timeGranularity} -> timeGranularity) (\s@DateTimeParameterDeclaration' {} a -> s {timeGranularity = a} :: DateTimeParameterDeclaration)

-- | The configuration that defines the default value of a @DateTime@
-- parameter when a value has not been set.
dateTimeParameterDeclaration_valueWhenUnset :: Lens.Lens' DateTimeParameterDeclaration (Prelude.Maybe DateTimeValueWhenUnsetConfiguration)
dateTimeParameterDeclaration_valueWhenUnset = Lens.lens (\DateTimeParameterDeclaration' {valueWhenUnset} -> valueWhenUnset) (\s@DateTimeParameterDeclaration' {} a -> s {valueWhenUnset = a} :: DateTimeParameterDeclaration)

-- | The name of the parameter that is being declared.
dateTimeParameterDeclaration_name :: Lens.Lens' DateTimeParameterDeclaration Prelude.Text
dateTimeParameterDeclaration_name = Lens.lens (\DateTimeParameterDeclaration' {name} -> name) (\s@DateTimeParameterDeclaration' {} a -> s {name = a} :: DateTimeParameterDeclaration)

instance Data.FromJSON DateTimeParameterDeclaration where
  parseJSON =
    Data.withObject
      "DateTimeParameterDeclaration"
      ( \x ->
          DateTimeParameterDeclaration'
            Prelude.<$> (x Data..:? "DefaultValues")
            Prelude.<*> (x Data..:? "TimeGranularity")
            Prelude.<*> (x Data..:? "ValueWhenUnset")
            Prelude.<*> (x Data..: "Name")
      )

instance
  Prelude.Hashable
    DateTimeParameterDeclaration
  where
  hashWithSalt _salt DateTimeParameterDeclaration' {..} =
    _salt `Prelude.hashWithSalt` defaultValues
      `Prelude.hashWithSalt` timeGranularity
      `Prelude.hashWithSalt` valueWhenUnset
      `Prelude.hashWithSalt` name

instance Prelude.NFData DateTimeParameterDeclaration where
  rnf DateTimeParameterDeclaration' {..} =
    Prelude.rnf defaultValues
      `Prelude.seq` Prelude.rnf timeGranularity
      `Prelude.seq` Prelude.rnf valueWhenUnset
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON DateTimeParameterDeclaration where
  toJSON DateTimeParameterDeclaration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DefaultValues" Data..=) Prelude.<$> defaultValues,
            ("TimeGranularity" Data..=)
              Prelude.<$> timeGranularity,
            ("ValueWhenUnset" Data..=)
              Prelude.<$> valueWhenUnset,
            Prelude.Just ("Name" Data..= name)
          ]
      )
