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
-- Module      : Amazonka.ConnectCases.Types.GetFieldResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.GetFieldResponse where

import Amazonka.ConnectCases.Types.FieldNamespace
import Amazonka.ConnectCases.Types.FieldType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Object to store detailed field information.
--
-- /See:/ 'newGetFieldResponse' smart constructor.
data GetFieldResponse = GetFieldResponse'
  { -- | Description of the field.
    description :: Prelude.Maybe Prelude.Text,
    -- | A map of of key-value pairs that represent tags on a resource. Tags are
    -- used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the field.
    fieldArn :: Prelude.Text,
    -- | Unique identifier of the field.
    fieldId :: Prelude.Text,
    -- | Name of the field.
    name :: Prelude.Text,
    -- | Namespace of the field.
    namespace :: FieldNamespace,
    -- | Type of the field.
    type' :: FieldType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFieldResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'getFieldResponse_description' - Description of the field.
--
-- 'tags', 'getFieldResponse_tags' - A map of of key-value pairs that represent tags on a resource. Tags are
-- used to organize, track, or control access for this resource.
--
-- 'fieldArn', 'getFieldResponse_fieldArn' - The Amazon Resource Name (ARN) of the field.
--
-- 'fieldId', 'getFieldResponse_fieldId' - Unique identifier of the field.
--
-- 'name', 'getFieldResponse_name' - Name of the field.
--
-- 'namespace', 'getFieldResponse_namespace' - Namespace of the field.
--
-- 'type'', 'getFieldResponse_type' - Type of the field.
newGetFieldResponse ::
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
  GetFieldResponse
newGetFieldResponse
  pFieldArn_
  pFieldId_
  pName_
  pNamespace_
  pType_ =
    GetFieldResponse'
      { description = Prelude.Nothing,
        tags = Prelude.Nothing,
        fieldArn = pFieldArn_,
        fieldId = pFieldId_,
        name = pName_,
        namespace = pNamespace_,
        type' = pType_
      }

-- | Description of the field.
getFieldResponse_description :: Lens.Lens' GetFieldResponse (Prelude.Maybe Prelude.Text)
getFieldResponse_description = Lens.lens (\GetFieldResponse' {description} -> description) (\s@GetFieldResponse' {} a -> s {description = a} :: GetFieldResponse)

-- | A map of of key-value pairs that represent tags on a resource. Tags are
-- used to organize, track, or control access for this resource.
getFieldResponse_tags :: Lens.Lens' GetFieldResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getFieldResponse_tags = Lens.lens (\GetFieldResponse' {tags} -> tags) (\s@GetFieldResponse' {} a -> s {tags = a} :: GetFieldResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the field.
getFieldResponse_fieldArn :: Lens.Lens' GetFieldResponse Prelude.Text
getFieldResponse_fieldArn = Lens.lens (\GetFieldResponse' {fieldArn} -> fieldArn) (\s@GetFieldResponse' {} a -> s {fieldArn = a} :: GetFieldResponse)

-- | Unique identifier of the field.
getFieldResponse_fieldId :: Lens.Lens' GetFieldResponse Prelude.Text
getFieldResponse_fieldId = Lens.lens (\GetFieldResponse' {fieldId} -> fieldId) (\s@GetFieldResponse' {} a -> s {fieldId = a} :: GetFieldResponse)

-- | Name of the field.
getFieldResponse_name :: Lens.Lens' GetFieldResponse Prelude.Text
getFieldResponse_name = Lens.lens (\GetFieldResponse' {name} -> name) (\s@GetFieldResponse' {} a -> s {name = a} :: GetFieldResponse)

-- | Namespace of the field.
getFieldResponse_namespace :: Lens.Lens' GetFieldResponse FieldNamespace
getFieldResponse_namespace = Lens.lens (\GetFieldResponse' {namespace} -> namespace) (\s@GetFieldResponse' {} a -> s {namespace = a} :: GetFieldResponse)

-- | Type of the field.
getFieldResponse_type :: Lens.Lens' GetFieldResponse FieldType
getFieldResponse_type = Lens.lens (\GetFieldResponse' {type'} -> type') (\s@GetFieldResponse' {} a -> s {type' = a} :: GetFieldResponse)

instance Data.FromJSON GetFieldResponse where
  parseJSON =
    Data.withObject
      "GetFieldResponse"
      ( \x ->
          GetFieldResponse'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "fieldArn")
            Prelude.<*> (x Data..: "fieldId")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "namespace")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable GetFieldResponse where
  hashWithSalt _salt GetFieldResponse' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` fieldArn
      `Prelude.hashWithSalt` fieldId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` type'

instance Prelude.NFData GetFieldResponse where
  rnf GetFieldResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf fieldArn
      `Prelude.seq` Prelude.rnf fieldId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf type'
