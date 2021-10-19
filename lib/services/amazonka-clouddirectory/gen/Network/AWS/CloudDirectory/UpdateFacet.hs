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
-- Module      : Network.AWS.CloudDirectory.UpdateFacet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.CloudDirectory.UpdateFacet
  ( -- * Creating a Request
    UpdateFacet (..),
    newUpdateFacet,

    -- * Request Lenses
    updateFacet_objectType,
    updateFacet_attributeUpdates,
    updateFacet_schemaArn,
    updateFacet_name,

    -- * Destructuring the Response
    UpdateFacetResponse (..),
    newUpdateFacetResponse,

    -- * Response Lenses
    updateFacetResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateFacet' smart constructor.
data UpdateFacet = UpdateFacet'
  { -- | The object type that is associated with the facet. See
    -- CreateFacetRequest$ObjectType for more details.
    objectType :: Prelude.Maybe ObjectType,
    -- | List of attributes that need to be updated in a given schema Facet. Each
    -- attribute is followed by @AttributeAction@, which specifies the type of
    -- update operation to perform.
    attributeUpdates :: Prelude.Maybe [FacetAttributeUpdate],
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
-- 'objectType', 'updateFacet_objectType' - The object type that is associated with the facet. See
-- CreateFacetRequest$ObjectType for more details.
--
-- 'attributeUpdates', 'updateFacet_attributeUpdates' - List of attributes that need to be updated in a given schema Facet. Each
-- attribute is followed by @AttributeAction@, which specifies the type of
-- update operation to perform.
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
    { objectType = Prelude.Nothing,
      attributeUpdates = Prelude.Nothing,
      schemaArn = pSchemaArn_,
      name = pName_
    }

-- | The object type that is associated with the facet. See
-- CreateFacetRequest$ObjectType for more details.
updateFacet_objectType :: Lens.Lens' UpdateFacet (Prelude.Maybe ObjectType)
updateFacet_objectType = Lens.lens (\UpdateFacet' {objectType} -> objectType) (\s@UpdateFacet' {} a -> s {objectType = a} :: UpdateFacet)

-- | List of attributes that need to be updated in a given schema Facet. Each
-- attribute is followed by @AttributeAction@, which specifies the type of
-- update operation to perform.
updateFacet_attributeUpdates :: Lens.Lens' UpdateFacet (Prelude.Maybe [FacetAttributeUpdate])
updateFacet_attributeUpdates = Lens.lens (\UpdateFacet' {attributeUpdates} -> attributeUpdates) (\s@UpdateFacet' {} a -> s {attributeUpdates = a} :: UpdateFacet) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) that is associated with the Facet. For
-- more information, see arns.
updateFacet_schemaArn :: Lens.Lens' UpdateFacet Prelude.Text
updateFacet_schemaArn = Lens.lens (\UpdateFacet' {schemaArn} -> schemaArn) (\s@UpdateFacet' {} a -> s {schemaArn = a} :: UpdateFacet)

-- | The name of the facet.
updateFacet_name :: Lens.Lens' UpdateFacet Prelude.Text
updateFacet_name = Lens.lens (\UpdateFacet' {name} -> name) (\s@UpdateFacet' {} a -> s {name = a} :: UpdateFacet)

instance Core.AWSRequest UpdateFacet where
  type AWSResponse UpdateFacet = UpdateFacetResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateFacetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFacet

instance Prelude.NFData UpdateFacet

instance Core.ToHeaders UpdateFacet where
  toHeaders UpdateFacet' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Core.=# schemaArn]

instance Core.ToJSON UpdateFacet where
  toJSON UpdateFacet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ObjectType" Core..=) Prelude.<$> objectType,
            ("AttributeUpdates" Core..=)
              Prelude.<$> attributeUpdates,
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath UpdateFacet where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/facet"

instance Core.ToQuery UpdateFacet where
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

instance Prelude.NFData UpdateFacetResponse
