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
-- Module      : Amazonka.Forecast.Types.SchemaAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.SchemaAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Forecast.Types.AttributeType
import qualified Amazonka.Prelude as Prelude

-- | An attribute of a schema, which defines a dataset field. A schema
-- attribute is required for every field in a dataset. The
-- <https://docs.aws.amazon.com/forecast/latest/dg/API_Schema.html Schema>
-- object contains an array of @SchemaAttribute@ objects.
--
-- /See:/ 'newSchemaAttribute' smart constructor.
data SchemaAttribute = SchemaAttribute'
  { -- | The data type of the field.
    --
    -- For a related time series dataset, other than date, item_id, and
    -- forecast dimensions attributes, all attributes should be of numerical
    -- type (integer\/float).
    attributeType :: Prelude.Maybe AttributeType,
    -- | The name of the dataset field.
    attributeName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SchemaAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeType', 'schemaAttribute_attributeType' - The data type of the field.
--
-- For a related time series dataset, other than date, item_id, and
-- forecast dimensions attributes, all attributes should be of numerical
-- type (integer\/float).
--
-- 'attributeName', 'schemaAttribute_attributeName' - The name of the dataset field.
newSchemaAttribute ::
  SchemaAttribute
newSchemaAttribute =
  SchemaAttribute'
    { attributeType = Prelude.Nothing,
      attributeName = Prelude.Nothing
    }

-- | The data type of the field.
--
-- For a related time series dataset, other than date, item_id, and
-- forecast dimensions attributes, all attributes should be of numerical
-- type (integer\/float).
schemaAttribute_attributeType :: Lens.Lens' SchemaAttribute (Prelude.Maybe AttributeType)
schemaAttribute_attributeType = Lens.lens (\SchemaAttribute' {attributeType} -> attributeType) (\s@SchemaAttribute' {} a -> s {attributeType = a} :: SchemaAttribute)

-- | The name of the dataset field.
schemaAttribute_attributeName :: Lens.Lens' SchemaAttribute (Prelude.Maybe Prelude.Text)
schemaAttribute_attributeName = Lens.lens (\SchemaAttribute' {attributeName} -> attributeName) (\s@SchemaAttribute' {} a -> s {attributeName = a} :: SchemaAttribute)

instance Core.FromJSON SchemaAttribute where
  parseJSON =
    Core.withObject
      "SchemaAttribute"
      ( \x ->
          SchemaAttribute'
            Prelude.<$> (x Core..:? "AttributeType")
            Prelude.<*> (x Core..:? "AttributeName")
      )

instance Prelude.Hashable SchemaAttribute where
  hashWithSalt _salt SchemaAttribute' {..} =
    _salt `Prelude.hashWithSalt` attributeType
      `Prelude.hashWithSalt` attributeName

instance Prelude.NFData SchemaAttribute where
  rnf SchemaAttribute' {..} =
    Prelude.rnf attributeType
      `Prelude.seq` Prelude.rnf attributeName

instance Core.ToJSON SchemaAttribute where
  toJSON SchemaAttribute' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AttributeType" Core..=) Prelude.<$> attributeType,
            ("AttributeName" Core..=) Prelude.<$> attributeName
          ]
      )
