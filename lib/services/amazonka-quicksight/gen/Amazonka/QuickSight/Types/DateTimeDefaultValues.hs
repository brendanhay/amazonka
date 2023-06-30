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
-- Module      : Amazonka.QuickSight.Types.DateTimeDefaultValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DateTimeDefaultValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DynamicDefaultValue
import Amazonka.QuickSight.Types.RollingDateConfiguration

-- | The default values of the @DateTimeParameterDeclaration@.
--
-- /See:/ 'newDateTimeDefaultValues' smart constructor.
data DateTimeDefaultValues = DateTimeDefaultValues'
  { -- | The dynamic value of the @DataTimeDefaultValues@. Different defaults are
    -- displayed according to users, groups, and values mapping.
    dynamicValue :: Prelude.Maybe DynamicDefaultValue,
    -- | The rolling date of the @DataTimeDefaultValues@. The date is determined
    -- from the dataset based on input expression.
    rollingDate :: Prelude.Maybe RollingDateConfiguration,
    -- | The static values of the @DataTimeDefaultValues@.
    staticValues :: Prelude.Maybe [Data.Sensitive Data.POSIX]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DateTimeDefaultValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dynamicValue', 'dateTimeDefaultValues_dynamicValue' - The dynamic value of the @DataTimeDefaultValues@. Different defaults are
-- displayed according to users, groups, and values mapping.
--
-- 'rollingDate', 'dateTimeDefaultValues_rollingDate' - The rolling date of the @DataTimeDefaultValues@. The date is determined
-- from the dataset based on input expression.
--
-- 'staticValues', 'dateTimeDefaultValues_staticValues' - The static values of the @DataTimeDefaultValues@.
newDateTimeDefaultValues ::
  DateTimeDefaultValues
newDateTimeDefaultValues =
  DateTimeDefaultValues'
    { dynamicValue =
        Prelude.Nothing,
      rollingDate = Prelude.Nothing,
      staticValues = Prelude.Nothing
    }

-- | The dynamic value of the @DataTimeDefaultValues@. Different defaults are
-- displayed according to users, groups, and values mapping.
dateTimeDefaultValues_dynamicValue :: Lens.Lens' DateTimeDefaultValues (Prelude.Maybe DynamicDefaultValue)
dateTimeDefaultValues_dynamicValue = Lens.lens (\DateTimeDefaultValues' {dynamicValue} -> dynamicValue) (\s@DateTimeDefaultValues' {} a -> s {dynamicValue = a} :: DateTimeDefaultValues)

-- | The rolling date of the @DataTimeDefaultValues@. The date is determined
-- from the dataset based on input expression.
dateTimeDefaultValues_rollingDate :: Lens.Lens' DateTimeDefaultValues (Prelude.Maybe RollingDateConfiguration)
dateTimeDefaultValues_rollingDate = Lens.lens (\DateTimeDefaultValues' {rollingDate} -> rollingDate) (\s@DateTimeDefaultValues' {} a -> s {rollingDate = a} :: DateTimeDefaultValues)

-- | The static values of the @DataTimeDefaultValues@.
dateTimeDefaultValues_staticValues :: Lens.Lens' DateTimeDefaultValues (Prelude.Maybe [Prelude.UTCTime])
dateTimeDefaultValues_staticValues = Lens.lens (\DateTimeDefaultValues' {staticValues} -> staticValues) (\s@DateTimeDefaultValues' {} a -> s {staticValues = a} :: DateTimeDefaultValues) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DateTimeDefaultValues where
  parseJSON =
    Data.withObject
      "DateTimeDefaultValues"
      ( \x ->
          DateTimeDefaultValues'
            Prelude.<$> (x Data..:? "DynamicValue")
            Prelude.<*> (x Data..:? "RollingDate")
            Prelude.<*> (x Data..:? "StaticValues" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable DateTimeDefaultValues where
  hashWithSalt _salt DateTimeDefaultValues' {..} =
    _salt
      `Prelude.hashWithSalt` dynamicValue
      `Prelude.hashWithSalt` rollingDate
      `Prelude.hashWithSalt` staticValues

instance Prelude.NFData DateTimeDefaultValues where
  rnf DateTimeDefaultValues' {..} =
    Prelude.rnf dynamicValue
      `Prelude.seq` Prelude.rnf rollingDate
      `Prelude.seq` Prelude.rnf staticValues

instance Data.ToJSON DateTimeDefaultValues where
  toJSON DateTimeDefaultValues' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DynamicValue" Data..=) Prelude.<$> dynamicValue,
            ("RollingDate" Data..=) Prelude.<$> rollingDate,
            ("StaticValues" Data..=) Prelude.<$> staticValues
          ]
      )
