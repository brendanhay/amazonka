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
-- Module      : Amazonka.CloudDirectory.UpdateFacet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Does the following:
--
-- 1.  Adds new @Attributes@, @Rules@, or @ObjectTypes@.
--
-- 2.  Updates existing @Attributes@, @Rules@, or @ObjectTypes@.
--
-- 3.  Deletes existing @Attributes@, @Rules@, or @ObjectTypes@.
module Amazonka.CloudDirectory.UpdateFacet
  ( -- * Creating a Request
    UpdateFacet (..),
    newUpdateFacet,

    -- * Request Lenses
    updateFacet_attributeUpdates,
    updateFacet_objectType,
    updateFacet_schemaArn,
    updateFacet_name,

    -- * Destructuring the Response
    UpdateFacetResponse (..),
    newUpdateFacetResponse,

    -- * Response Lenses
    updateFacetResponse_httpStatus,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFacet' smart constructor.
data UpdateFacet = UpdateFacet'
  { -- | List of attributes that need to be updated in a given schema Facet. Each
    -- attribute is followed by @AttributeAction@, which specifies the type of
    -- update operation to perform.
    attributeUpdates :: Prelude.Maybe [FacetAttributeUpdate],
    -- | The object type that is associated with the facet. See
    -- CreateFacetRequest$ObjectType for more details.
    objectType :: Prelude.Maybe ObjectType,
    -- | The Amazon Resource Name (ARN) that is associated with the Facet. For
    -- more information, see arns.
    schemaArn :: Prelude.Text,
    -- | The name of the facet.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFacet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeUpdates', 'updateFacet_attributeUpdates' - List of attributes that need to be updated in a given schema Facet. Each
-- attribute is followed by @AttributeAction@, which specifies the type of
-- update operation to perform.
--
-- 'objectType', 'updateFacet_objectType' - The object type that is associated with the facet. See
-- CreateFacetRequest$ObjectType for more details.
--
-- 'schemaArn', 'updateFacet_schemaArn' - The Amazon Resource Name (ARN) that is associated with the Facet. For
-- more information, see arns.
--
-- 'name', 'updateFacet_name' - The name of the facet.
newUpdateFacet ::
  -- | 'schemaArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  UpdateFacet
newUpdateFacet pSchemaArn_ pName_ =
  UpdateFacet'
    { attributeUpdates = Prelude.Nothing,
      objectType = Prelude.Nothing,
      schemaArn = pSchemaArn_,
      name = pName_
    }

-- | List of attributes that need to be updated in a given schema Facet. Each
-- attribute is followed by @AttributeAction@, which specifies the type of
-- update operation to perform.
updateFacet_attributeUpdates :: Lens.Lens' UpdateFacet (Prelude.Maybe [FacetAttributeUpdate])
updateFacet_attributeUpdates = Lens.lens (\UpdateFacet' {attributeUpdates} -> attributeUpdates) (\s@UpdateFacet' {} a -> s {attributeUpdates = a} :: UpdateFacet) Prelude.. Lens.mapping Lens.coerced

-- | The object type that is associated with the facet. See
-- CreateFacetRequest$ObjectType for more details.
updateFacet_objectType :: Lens.Lens' UpdateFacet (Prelude.Maybe ObjectType)
updateFacet_objectType = Lens.lens (\UpdateFacet' {objectType} -> objectType) (\s@UpdateFacet' {} a -> s {objectType = a} :: UpdateFacet)

-- | The Amazon Resource Name (ARN) that is associated with the Facet. For
-- more information, see arns.
updateFacet_schemaArn :: Lens.Lens' UpdateFacet Prelude.Text
updateFacet_schemaArn = Lens.lens (\UpdateFacet' {schemaArn} -> schemaArn) (\s@UpdateFacet' {} a -> s {schemaArn = a} :: UpdateFacet)

-- | The name of the facet.
updateFacet_name :: Lens.Lens' UpdateFacet Prelude.Text
updateFacet_name = Lens.lens (\UpdateFacet' {name} -> name) (\s@UpdateFacet' {} a -> s {name = a} :: UpdateFacet)

instance Core.AWSRequest UpdateFacet where
  type AWSResponse UpdateFacet = UpdateFacetResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateFacetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFacet where
  hashWithSalt _salt UpdateFacet' {..} =
    _salt
      `Prelude.hashWithSalt` attributeUpdates
      `Prelude.hashWithSalt` objectType
      `Prelude.hashWithSalt` schemaArn
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateFacet where
  rnf UpdateFacet' {..} =
    Prelude.rnf attributeUpdates
      `Prelude.seq` Prelude.rnf objectType
      `Prelude.seq` Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateFacet where
  toHeaders UpdateFacet' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Data.=# schemaArn]

instance Data.ToJSON UpdateFacet where
  toJSON UpdateFacet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttributeUpdates" Data..=)
              Prelude.<$> attributeUpdates,
            ("ObjectType" Data..=) Prelude.<$> objectType,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath UpdateFacet where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/facet"

instance Data.ToQuery UpdateFacet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFacetResponse' smart constructor.
data UpdateFacetResponse = UpdateFacetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFacetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateFacetResponse_httpStatus' - The response's http status code.
newUpdateFacetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateFacetResponse
newUpdateFacetResponse pHttpStatus_ =
  UpdateFacetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateFacetResponse_httpStatus :: Lens.Lens' UpdateFacetResponse Prelude.Int
updateFacetResponse_httpStatus = Lens.lens (\UpdateFacetResponse' {httpStatus} -> httpStatus) (\s@UpdateFacetResponse' {} a -> s {httpStatus = a} :: UpdateFacetResponse)

instance Prelude.NFData UpdateFacetResponse where
  rnf UpdateFacetResponse' {..} = Prelude.rnf httpStatus
