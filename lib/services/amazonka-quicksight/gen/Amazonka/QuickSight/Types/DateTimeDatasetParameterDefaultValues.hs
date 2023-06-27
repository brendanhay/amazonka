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
-- Module      : Amazonka.QuickSight.Types.DateTimeDatasetParameterDefaultValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DateTimeDatasetParameterDefaultValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The default values of a date time parameter.
--
-- /See:/ 'newDateTimeDatasetParameterDefaultValues' smart constructor.
data DateTimeDatasetParameterDefaultValues = DateTimeDatasetParameterDefaultValues'
  { -- | A list of static default values for a given date time parameter.
    staticValues :: Prelude.Maybe (Prelude.NonEmpty Data.POSIX)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DateTimeDatasetParameterDefaultValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'staticValues', 'dateTimeDatasetParameterDefaultValues_staticValues' - A list of static default values for a given date time parameter.
newDateTimeDatasetParameterDefaultValues ::
  DateTimeDatasetParameterDefaultValues
newDateTimeDatasetParameterDefaultValues =
  DateTimeDatasetParameterDefaultValues'
    { staticValues =
        Prelude.Nothing
    }

-- | A list of static default values for a given date time parameter.
dateTimeDatasetParameterDefaultValues_staticValues :: Lens.Lens' DateTimeDatasetParameterDefaultValues (Prelude.Maybe (Prelude.NonEmpty Prelude.UTCTime))
dateTimeDatasetParameterDefaultValues_staticValues = Lens.lens (\DateTimeDatasetParameterDefaultValues' {staticValues} -> staticValues) (\s@DateTimeDatasetParameterDefaultValues' {} a -> s {staticValues = a} :: DateTimeDatasetParameterDefaultValues) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    DateTimeDatasetParameterDefaultValues
  where
  parseJSON =
    Data.withObject
      "DateTimeDatasetParameterDefaultValues"
      ( \x ->
          DateTimeDatasetParameterDefaultValues'
            Prelude.<$> (x Data..:? "StaticValues")
      )

instance
  Prelude.Hashable
    DateTimeDatasetParameterDefaultValues
  where
  hashWithSalt
    _salt
    DateTimeDatasetParameterDefaultValues' {..} =
      _salt `Prelude.hashWithSalt` staticValues

instance
  Prelude.NFData
    DateTimeDatasetParameterDefaultValues
  where
  rnf DateTimeDatasetParameterDefaultValues' {..} =
    Prelude.rnf staticValues

instance
  Data.ToJSON
    DateTimeDatasetParameterDefaultValues
  where
  toJSON DateTimeDatasetParameterDefaultValues' {..} =
    Data.object
      ( Prelude.catMaybes
          [("StaticValues" Data..=) Prelude.<$> staticValues]
      )
