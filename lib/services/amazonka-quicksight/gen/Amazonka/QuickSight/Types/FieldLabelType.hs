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
-- Module      : Amazonka.QuickSight.Types.FieldLabelType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FieldLabelType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Visibility

-- | The field label type.
--
-- /See:/ 'newFieldLabelType' smart constructor.
data FieldLabelType = FieldLabelType'
  { -- | Indicates the field that is targeted by the field label.
    fieldId :: Prelude.Maybe Prelude.Text,
    -- | The visibility of the field label.
    visibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldLabelType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldId', 'fieldLabelType_fieldId' - Indicates the field that is targeted by the field label.
--
-- 'visibility', 'fieldLabelType_visibility' - The visibility of the field label.
newFieldLabelType ::
  FieldLabelType
newFieldLabelType =
  FieldLabelType'
    { fieldId = Prelude.Nothing,
      visibility = Prelude.Nothing
    }

-- | Indicates the field that is targeted by the field label.
fieldLabelType_fieldId :: Lens.Lens' FieldLabelType (Prelude.Maybe Prelude.Text)
fieldLabelType_fieldId = Lens.lens (\FieldLabelType' {fieldId} -> fieldId) (\s@FieldLabelType' {} a -> s {fieldId = a} :: FieldLabelType)

-- | The visibility of the field label.
fieldLabelType_visibility :: Lens.Lens' FieldLabelType (Prelude.Maybe Visibility)
fieldLabelType_visibility = Lens.lens (\FieldLabelType' {visibility} -> visibility) (\s@FieldLabelType' {} a -> s {visibility = a} :: FieldLabelType)

instance Data.FromJSON FieldLabelType where
  parseJSON =
    Data.withObject
      "FieldLabelType"
      ( \x ->
          FieldLabelType'
            Prelude.<$> (x Data..:? "FieldId")
            Prelude.<*> (x Data..:? "Visibility")
      )

instance Prelude.Hashable FieldLabelType where
  hashWithSalt _salt FieldLabelType' {..} =
    _salt
      `Prelude.hashWithSalt` fieldId
      `Prelude.hashWithSalt` visibility

instance Prelude.NFData FieldLabelType where
  rnf FieldLabelType' {..} =
    Prelude.rnf fieldId
      `Prelude.seq` Prelude.rnf visibility

instance Data.ToJSON FieldLabelType where
  toJSON FieldLabelType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FieldId" Data..=) Prelude.<$> fieldId,
            ("Visibility" Data..=) Prelude.<$> visibility
          ]
      )
