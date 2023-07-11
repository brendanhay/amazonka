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
-- Module      : Amazonka.ConnectCases.Types.FieldSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.FieldSummary where

import Amazonka.ConnectCases.Types.FieldNamespace
import Amazonka.ConnectCases.Types.FieldType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Object for the summarized details of the field.
--
-- /See:/ 'newFieldSummary' smart constructor.
data FieldSummary = FieldSummary'
  { -- | The Amazon Resource Name (ARN) of the field.
    fieldArn :: Prelude.Text,
    -- | The unique identifier of a field.
    fieldId :: Prelude.Text,
    -- | Name of the field.
    name :: Prelude.Text,
    -- | The namespace of a field.
    namespace :: FieldNamespace,
    -- | The type of a field.
    type' :: FieldType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldArn', 'fieldSummary_fieldArn' - The Amazon Resource Name (ARN) of the field.
--
-- 'fieldId', 'fieldSummary_fieldId' - The unique identifier of a field.
--
-- 'name', 'fieldSummary_name' - Name of the field.
--
-- 'namespace', 'fieldSummary_namespace' - The namespace of a field.
--
-- 'type'', 'fieldSummary_type' - The type of a field.
newFieldSummary ::
  -- | 'fieldArn'
  Prelude.Text ->
  -- | 'fieldId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'namespace'
  FieldNamespace ->
  -- | 'type''
  FieldType ->
  FieldSummary
newFieldSummary
  pFieldArn_
  pFieldId_
  pName_
  pNamespace_
  pType_ =
    FieldSummary'
      { fieldArn = pFieldArn_,
        fieldId = pFieldId_,
        name = pName_,
        namespace = pNamespace_,
        type' = pType_
      }

-- | The Amazon Resource Name (ARN) of the field.
fieldSummary_fieldArn :: Lens.Lens' FieldSummary Prelude.Text
fieldSummary_fieldArn = Lens.lens (\FieldSummary' {fieldArn} -> fieldArn) (\s@FieldSummary' {} a -> s {fieldArn = a} :: FieldSummary)

-- | The unique identifier of a field.
fieldSummary_fieldId :: Lens.Lens' FieldSummary Prelude.Text
fieldSummary_fieldId = Lens.lens (\FieldSummary' {fieldId} -> fieldId) (\s@FieldSummary' {} a -> s {fieldId = a} :: FieldSummary)

-- | Name of the field.
fieldSummary_name :: Lens.Lens' FieldSummary Prelude.Text
fieldSummary_name = Lens.lens (\FieldSummary' {name} -> name) (\s@FieldSummary' {} a -> s {name = a} :: FieldSummary)

-- | The namespace of a field.
fieldSummary_namespace :: Lens.Lens' FieldSummary FieldNamespace
fieldSummary_namespace = Lens.lens (\FieldSummary' {namespace} -> namespace) (\s@FieldSummary' {} a -> s {namespace = a} :: FieldSummary)

-- | The type of a field.
fieldSummary_type :: Lens.Lens' FieldSummary FieldType
fieldSummary_type = Lens.lens (\FieldSummary' {type'} -> type') (\s@FieldSummary' {} a -> s {type' = a} :: FieldSummary)

instance Data.FromJSON FieldSummary where
  parseJSON =
    Data.withObject
      "FieldSummary"
      ( \x ->
          FieldSummary'
            Prelude.<$> (x Data..: "fieldArn")
            Prelude.<*> (x Data..: "fieldId")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "namespace")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable FieldSummary where
  hashWithSalt _salt FieldSummary' {..} =
    _salt
      `Prelude.hashWithSalt` fieldArn
      `Prelude.hashWithSalt` fieldId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` type'

instance Prelude.NFData FieldSummary where
  rnf FieldSummary' {..} =
    Prelude.rnf fieldArn
      `Prelude.seq` Prelude.rnf fieldId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf type'
