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
-- Module      : Amazonka.ConnectCases.Types.RequiredField
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.RequiredField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | List of fields that must have a value provided to create a case.
--
-- /See:/ 'newRequiredField' smart constructor.
data RequiredField = RequiredField'
  { -- | Unique identifier of a field.
    fieldId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequiredField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldId', 'requiredField_fieldId' - Unique identifier of a field.
newRequiredField ::
  -- | 'fieldId'
  Prelude.Text ->
  RequiredField
newRequiredField pFieldId_ =
  RequiredField' {fieldId = pFieldId_}

-- | Unique identifier of a field.
requiredField_fieldId :: Lens.Lens' RequiredField Prelude.Text
requiredField_fieldId = Lens.lens (\RequiredField' {fieldId} -> fieldId) (\s@RequiredField' {} a -> s {fieldId = a} :: RequiredField)

instance Data.FromJSON RequiredField where
  parseJSON =
    Data.withObject
      "RequiredField"
      ( \x ->
          RequiredField' Prelude.<$> (x Data..: "fieldId")
      )

instance Prelude.Hashable RequiredField where
  hashWithSalt _salt RequiredField' {..} =
    _salt `Prelude.hashWithSalt` fieldId

instance Prelude.NFData RequiredField where
  rnf RequiredField' {..} = Prelude.rnf fieldId

instance Data.ToJSON RequiredField where
  toJSON RequiredField' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("fieldId" Data..= fieldId)]
      )
