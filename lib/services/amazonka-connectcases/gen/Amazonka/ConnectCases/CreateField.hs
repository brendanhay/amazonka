{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ConnectCases.CreateField
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a field in the Cases domain. This field is used to define the
-- case object model (that is, defines what data can be captured on cases)
-- in a Cases domain.
module Amazonka.ConnectCases.CreateField
  ( -- * Creating a Request
    CreateField (..),
    newCreateField,

    -- * Request Lenses
    createField_description,
    createField_domainId,
    createField_name,
    createField_type,

    -- * Destructuring the Response
    CreateFieldResponse (..),
    newCreateFieldResponse,

    -- * Response Lenses
    createFieldResponse_httpStatus,
    createFieldResponse_fieldArn,
    createFieldResponse_fieldId,
  )
where

import Amazonka.ConnectCases.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateField' smart constructor.
data CreateField = CreateField'
  { -- | The description of the field.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the Cases domain.
    domainId :: Prelude.Text,
    -- | The name of the field.
    name :: Prelude.Text,
    -- | Defines the data type, some system constraints, and default display of
    -- the field.
    type' :: FieldType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createField_description' - The description of the field.
--
-- 'domainId', 'createField_domainId' - The unique identifier of the Cases domain.
--
-- 'name', 'createField_name' - The name of the field.
--
-- 'type'', 'createField_type' - Defines the data type, some system constraints, and default display of
-- the field.
newCreateField ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  FieldType ->
  CreateField
newCreateField pDomainId_ pName_ pType_ =
  CreateField'
    { description = Prelude.Nothing,
      domainId = pDomainId_,
      name = pName_,
      type' = pType_
    }

-- | The description of the field.
createField_description :: Lens.Lens' CreateField (Prelude.Maybe Prelude.Text)
createField_description = Lens.lens (\CreateField' {description} -> description) (\s@CreateField' {} a -> s {description = a} :: CreateField)

-- | The unique identifier of the Cases domain.
createField_domainId :: Lens.Lens' CreateField Prelude.Text
createField_domainId = Lens.lens (\CreateField' {domainId} -> domainId) (\s@CreateField' {} a -> s {domainId = a} :: CreateField)

-- | The name of the field.
createField_name :: Lens.Lens' CreateField Prelude.Text
createField_name = Lens.lens (\CreateField' {name} -> name) (\s@CreateField' {} a -> s {name = a} :: CreateField)

-- | Defines the data type, some system constraints, and default display of
-- the field.
createField_type :: Lens.Lens' CreateField FieldType
createField_type = Lens.lens (\CreateField' {type'} -> type') (\s@CreateField' {} a -> s {type' = a} :: CreateField)

instance Core.AWSRequest CreateField where
  type AWSResponse CreateField = CreateFieldResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFieldResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "fieldArn")
            Prelude.<*> (x Core..:> "fieldId")
      )

instance Prelude.Hashable CreateField where
  hashWithSalt _salt CreateField' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateField where
  rnf CreateField' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'

instance Core.ToHeaders CreateField where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateField where
  toJSON CreateField' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("description" Core..=) Prelude.<$> description,
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("type" Core..= type')
          ]
      )

instance Core.ToPath CreateField where
  toPath CreateField' {..} =
    Prelude.mconcat
      ["/domains/", Core.toBS domainId, "/fields"]

instance Core.ToQuery CreateField where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFieldResponse' smart constructor.
data CreateFieldResponse = CreateFieldResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the field.
    fieldArn :: Prelude.Text,
    -- | The unique identifier of a field.
    fieldId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFieldResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createFieldResponse_httpStatus' - The response's http status code.
--
-- 'fieldArn', 'createFieldResponse_fieldArn' - The Amazon Resource Name (ARN) of the field.
--
-- 'fieldId', 'createFieldResponse_fieldId' - The unique identifier of a field.
newCreateFieldResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'fieldArn'
  Prelude.Text ->
  -- | 'fieldId'
  Prelude.Text ->
  CreateFieldResponse
newCreateFieldResponse
  pHttpStatus_
  pFieldArn_
  pFieldId_ =
    CreateFieldResponse'
      { httpStatus = pHttpStatus_,
        fieldArn = pFieldArn_,
        fieldId = pFieldId_
      }

-- | The response's http status code.
createFieldResponse_httpStatus :: Lens.Lens' CreateFieldResponse Prelude.Int
createFieldResponse_httpStatus = Lens.lens (\CreateFieldResponse' {httpStatus} -> httpStatus) (\s@CreateFieldResponse' {} a -> s {httpStatus = a} :: CreateFieldResponse)

-- | The Amazon Resource Name (ARN) of the field.
createFieldResponse_fieldArn :: Lens.Lens' CreateFieldResponse Prelude.Text
createFieldResponse_fieldArn = Lens.lens (\CreateFieldResponse' {fieldArn} -> fieldArn) (\s@CreateFieldResponse' {} a -> s {fieldArn = a} :: CreateFieldResponse)

-- | The unique identifier of a field.
createFieldResponse_fieldId :: Lens.Lens' CreateFieldResponse Prelude.Text
createFieldResponse_fieldId = Lens.lens (\CreateFieldResponse' {fieldId} -> fieldId) (\s@CreateFieldResponse' {} a -> s {fieldId = a} :: CreateFieldResponse)

instance Prelude.NFData CreateFieldResponse where
  rnf CreateFieldResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf fieldArn
      `Prelude.seq` Prelude.rnf fieldId
