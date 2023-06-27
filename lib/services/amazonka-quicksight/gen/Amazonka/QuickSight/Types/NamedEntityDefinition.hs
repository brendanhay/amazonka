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
-- Module      : Amazonka.QuickSight.Types.NamedEntityDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NamedEntityDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.NamedEntityDefinitionMetric
import Amazonka.QuickSight.Types.PropertyRole
import Amazonka.QuickSight.Types.PropertyUsage

-- | A structure that represents a named entity.
--
-- /See:/ 'newNamedEntityDefinition' smart constructor.
data NamedEntityDefinition = NamedEntityDefinition'
  { -- | The name of the entity.
    fieldName :: Prelude.Maybe Prelude.Text,
    -- | The definition of a metric.
    metric :: Prelude.Maybe NamedEntityDefinitionMetric,
    -- | The property name to be used for the named entity.
    propertyName :: Prelude.Maybe Prelude.Text,
    -- | The property role. Valid values for this structure are @PRIMARY@ and
    -- @ID@.
    propertyRole :: Prelude.Maybe PropertyRole,
    -- | The property usage. Valid values for this structure are @INHERIT@,
    -- @DIMENSION@, and @MEASURE@.
    propertyUsage :: Prelude.Maybe PropertyUsage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NamedEntityDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldName', 'namedEntityDefinition_fieldName' - The name of the entity.
--
-- 'metric', 'namedEntityDefinition_metric' - The definition of a metric.
--
-- 'propertyName', 'namedEntityDefinition_propertyName' - The property name to be used for the named entity.
--
-- 'propertyRole', 'namedEntityDefinition_propertyRole' - The property role. Valid values for this structure are @PRIMARY@ and
-- @ID@.
--
-- 'propertyUsage', 'namedEntityDefinition_propertyUsage' - The property usage. Valid values for this structure are @INHERIT@,
-- @DIMENSION@, and @MEASURE@.
newNamedEntityDefinition ::
  NamedEntityDefinition
newNamedEntityDefinition =
  NamedEntityDefinition'
    { fieldName = Prelude.Nothing,
      metric = Prelude.Nothing,
      propertyName = Prelude.Nothing,
      propertyRole = Prelude.Nothing,
      propertyUsage = Prelude.Nothing
    }

-- | The name of the entity.
namedEntityDefinition_fieldName :: Lens.Lens' NamedEntityDefinition (Prelude.Maybe Prelude.Text)
namedEntityDefinition_fieldName = Lens.lens (\NamedEntityDefinition' {fieldName} -> fieldName) (\s@NamedEntityDefinition' {} a -> s {fieldName = a} :: NamedEntityDefinition)

-- | The definition of a metric.
namedEntityDefinition_metric :: Lens.Lens' NamedEntityDefinition (Prelude.Maybe NamedEntityDefinitionMetric)
namedEntityDefinition_metric = Lens.lens (\NamedEntityDefinition' {metric} -> metric) (\s@NamedEntityDefinition' {} a -> s {metric = a} :: NamedEntityDefinition)

-- | The property name to be used for the named entity.
namedEntityDefinition_propertyName :: Lens.Lens' NamedEntityDefinition (Prelude.Maybe Prelude.Text)
namedEntityDefinition_propertyName = Lens.lens (\NamedEntityDefinition' {propertyName} -> propertyName) (\s@NamedEntityDefinition' {} a -> s {propertyName = a} :: NamedEntityDefinition)

-- | The property role. Valid values for this structure are @PRIMARY@ and
-- @ID@.
namedEntityDefinition_propertyRole :: Lens.Lens' NamedEntityDefinition (Prelude.Maybe PropertyRole)
namedEntityDefinition_propertyRole = Lens.lens (\NamedEntityDefinition' {propertyRole} -> propertyRole) (\s@NamedEntityDefinition' {} a -> s {propertyRole = a} :: NamedEntityDefinition)

-- | The property usage. Valid values for this structure are @INHERIT@,
-- @DIMENSION@, and @MEASURE@.
namedEntityDefinition_propertyUsage :: Lens.Lens' NamedEntityDefinition (Prelude.Maybe PropertyUsage)
namedEntityDefinition_propertyUsage = Lens.lens (\NamedEntityDefinition' {propertyUsage} -> propertyUsage) (\s@NamedEntityDefinition' {} a -> s {propertyUsage = a} :: NamedEntityDefinition)

instance Data.FromJSON NamedEntityDefinition where
  parseJSON =
    Data.withObject
      "NamedEntityDefinition"
      ( \x ->
          NamedEntityDefinition'
            Prelude.<$> (x Data..:? "FieldName")
            Prelude.<*> (x Data..:? "Metric")
            Prelude.<*> (x Data..:? "PropertyName")
            Prelude.<*> (x Data..:? "PropertyRole")
            Prelude.<*> (x Data..:? "PropertyUsage")
      )

instance Prelude.Hashable NamedEntityDefinition where
  hashWithSalt _salt NamedEntityDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` fieldName
      `Prelude.hashWithSalt` metric
      `Prelude.hashWithSalt` propertyName
      `Prelude.hashWithSalt` propertyRole
      `Prelude.hashWithSalt` propertyUsage

instance Prelude.NFData NamedEntityDefinition where
  rnf NamedEntityDefinition' {..} =
    Prelude.rnf fieldName
      `Prelude.seq` Prelude.rnf metric
      `Prelude.seq` Prelude.rnf propertyName
      `Prelude.seq` Prelude.rnf propertyRole
      `Prelude.seq` Prelude.rnf propertyUsage

instance Data.ToJSON NamedEntityDefinition where
  toJSON NamedEntityDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FieldName" Data..=) Prelude.<$> fieldName,
            ("Metric" Data..=) Prelude.<$> metric,
            ("PropertyName" Data..=) Prelude.<$> propertyName,
            ("PropertyRole" Data..=) Prelude.<$> propertyRole,
            ("PropertyUsage" Data..=) Prelude.<$> propertyUsage
          ]
      )
