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
-- Module      : Amazonka.ConnectCases.UpdateField
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the properties of an existing field.
module Amazonka.ConnectCases.UpdateField
  ( -- * Creating a Request
    UpdateField (..),
    newUpdateField,

    -- * Request Lenses
    updateField_description,
    updateField_name,
    updateField_domainId,
    updateField_fieldId,

    -- * Destructuring the Response
    UpdateFieldResponse (..),
    newUpdateFieldResponse,

    -- * Response Lenses
    updateFieldResponse_httpStatus,
  )
where

import Amazonka.ConnectCases.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateField' smart constructor.
data UpdateField = UpdateField'
  { -- | The description of a field.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the field.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the Cases domain.
    domainId :: Prelude.Text,
    -- | The unique identifier of a field.
    fieldId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateField_description' - The description of a field.
--
-- 'name', 'updateField_name' - The name of the field.
--
-- 'domainId', 'updateField_domainId' - The unique identifier of the Cases domain.
--
-- 'fieldId', 'updateField_fieldId' - The unique identifier of a field.
newUpdateField ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'fieldId'
  Prelude.Text ->
  UpdateField
newUpdateField pDomainId_ pFieldId_ =
  UpdateField'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      domainId = pDomainId_,
      fieldId = pFieldId_
    }

-- | The description of a field.
updateField_description :: Lens.Lens' UpdateField (Prelude.Maybe Prelude.Text)
updateField_description = Lens.lens (\UpdateField' {description} -> description) (\s@UpdateField' {} a -> s {description = a} :: UpdateField)

-- | The name of the field.
updateField_name :: Lens.Lens' UpdateField (Prelude.Maybe Prelude.Text)
updateField_name = Lens.lens (\UpdateField' {name} -> name) (\s@UpdateField' {} a -> s {name = a} :: UpdateField)

-- | The unique identifier of the Cases domain.
updateField_domainId :: Lens.Lens' UpdateField Prelude.Text
updateField_domainId = Lens.lens (\UpdateField' {domainId} -> domainId) (\s@UpdateField' {} a -> s {domainId = a} :: UpdateField)

-- | The unique identifier of a field.
updateField_fieldId :: Lens.Lens' UpdateField Prelude.Text
updateField_fieldId = Lens.lens (\UpdateField' {fieldId} -> fieldId) (\s@UpdateField' {} a -> s {fieldId = a} :: UpdateField)

instance Core.AWSRequest UpdateField where
  type AWSResponse UpdateField = UpdateFieldResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateFieldResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateField where
  hashWithSalt _salt UpdateField' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` fieldId

instance Prelude.NFData UpdateField where
  rnf UpdateField' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf fieldId

instance Data.ToHeaders UpdateField where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateField where
  toJSON UpdateField' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("name" Data..=) Prelude.<$> name
          ]
      )

instance Data.ToPath UpdateField where
  toPath UpdateField' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainId,
        "/fields/",
        Data.toBS fieldId
      ]

instance Data.ToQuery UpdateField where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFieldResponse' smart constructor.
data UpdateFieldResponse = UpdateFieldResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFieldResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateFieldResponse_httpStatus' - The response's http status code.
newUpdateFieldResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFieldResponse
newUpdateFieldResponse pHttpStatus_ =
  UpdateFieldResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateFieldResponse_httpStatus :: Lens.Lens' UpdateFieldResponse Prelude.Int
updateFieldResponse_httpStatus = Lens.lens (\UpdateFieldResponse' {httpStatus} -> httpStatus) (\s@UpdateFieldResponse' {} a -> s {httpStatus = a} :: UpdateFieldResponse)

instance Prelude.NFData UpdateFieldResponse where
  rnf UpdateFieldResponse' {..} = Prelude.rnf httpStatus
