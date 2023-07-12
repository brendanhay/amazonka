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
-- Module      : Amazonka.QuickSight.Types.UnaggregatedField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.UnaggregatedField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnIdentifier
import Amazonka.QuickSight.Types.FormatConfiguration

-- | The unaggregated field for a table.
--
-- /See:/ 'newUnaggregatedField' smart constructor.
data UnaggregatedField = UnaggregatedField'
  { -- | The format configuration of the field.
    formatConfiguration :: Prelude.Maybe FormatConfiguration,
    -- | The custom field ID.
    fieldId :: Prelude.Text,
    -- | The column that is used in the @UnaggregatedField@.
    column :: ColumnIdentifier
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnaggregatedField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'formatConfiguration', 'unaggregatedField_formatConfiguration' - The format configuration of the field.
--
-- 'fieldId', 'unaggregatedField_fieldId' - The custom field ID.
--
-- 'column', 'unaggregatedField_column' - The column that is used in the @UnaggregatedField@.
newUnaggregatedField ::
  -- | 'fieldId'
  Prelude.Text ->
  -- | 'column'
  ColumnIdentifier ->
  UnaggregatedField
newUnaggregatedField pFieldId_ pColumn_ =
  UnaggregatedField'
    { formatConfiguration =
        Prelude.Nothing,
      fieldId = pFieldId_,
      column = pColumn_
    }

-- | The format configuration of the field.
unaggregatedField_formatConfiguration :: Lens.Lens' UnaggregatedField (Prelude.Maybe FormatConfiguration)
unaggregatedField_formatConfiguration = Lens.lens (\UnaggregatedField' {formatConfiguration} -> formatConfiguration) (\s@UnaggregatedField' {} a -> s {formatConfiguration = a} :: UnaggregatedField)

-- | The custom field ID.
unaggregatedField_fieldId :: Lens.Lens' UnaggregatedField Prelude.Text
unaggregatedField_fieldId = Lens.lens (\UnaggregatedField' {fieldId} -> fieldId) (\s@UnaggregatedField' {} a -> s {fieldId = a} :: UnaggregatedField)

-- | The column that is used in the @UnaggregatedField@.
unaggregatedField_column :: Lens.Lens' UnaggregatedField ColumnIdentifier
unaggregatedField_column = Lens.lens (\UnaggregatedField' {column} -> column) (\s@UnaggregatedField' {} a -> s {column = a} :: UnaggregatedField)

instance Data.FromJSON UnaggregatedField where
  parseJSON =
    Data.withObject
      "UnaggregatedField"
      ( \x ->
          UnaggregatedField'
            Prelude.<$> (x Data..:? "FormatConfiguration")
            Prelude.<*> (x Data..: "FieldId")
            Prelude.<*> (x Data..: "Column")
      )

instance Prelude.Hashable UnaggregatedField where
  hashWithSalt _salt UnaggregatedField' {..} =
    _salt
      `Prelude.hashWithSalt` formatConfiguration
      `Prelude.hashWithSalt` fieldId
      `Prelude.hashWithSalt` column

instance Prelude.NFData UnaggregatedField where
  rnf UnaggregatedField' {..} =
    Prelude.rnf formatConfiguration
      `Prelude.seq` Prelude.rnf fieldId
      `Prelude.seq` Prelude.rnf column

instance Data.ToJSON UnaggregatedField where
  toJSON UnaggregatedField' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FormatConfiguration" Data..=)
              Prelude.<$> formatConfiguration,
            Prelude.Just ("FieldId" Data..= fieldId),
            Prelude.Just ("Column" Data..= column)
          ]
      )
