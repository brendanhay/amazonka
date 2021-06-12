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

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateFacet' smart constructor.
data UpdateFacet = UpdateFacet'
  { -- | List of attributes that need to be updated in a given schema Facet. Each
    -- attribute is followed by @AttributeAction@, which specifies the type of
    -- update operation to perform.
    attributeUpdates :: Core.Maybe [FacetAttributeUpdate],
    -- | The object type that is associated with the facet. See
    -- CreateFacetRequest$ObjectType for more details.
    objectType :: Core.Maybe ObjectType,
    -- | The Amazon Resource Name (ARN) that is associated with the Facet. For
    -- more information, see arns.
    schemaArn :: Core.Text,
    -- | The name of the facet.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'name'
  Core.Text ->
  UpdateFacet
newUpdateFacet pSchemaArn_ pName_ =
  UpdateFacet'
    { attributeUpdates = Core.Nothing,
      objectType = Core.Nothing,
      schemaArn = pSchemaArn_,
      name = pName_
    }

-- | List of attributes that need to be updated in a given schema Facet. Each
-- attribute is followed by @AttributeAction@, which specifies the type of
-- update operation to perform.
updateFacet_attributeUpdates :: Lens.Lens' UpdateFacet (Core.Maybe [FacetAttributeUpdate])
updateFacet_attributeUpdates = Lens.lens (\UpdateFacet' {attributeUpdates} -> attributeUpdates) (\s@UpdateFacet' {} a -> s {attributeUpdates = a} :: UpdateFacet) Core.. Lens.mapping Lens._Coerce

-- | The object type that is associated with the facet. See
-- CreateFacetRequest$ObjectType for more details.
updateFacet_objectType :: Lens.Lens' UpdateFacet (Core.Maybe ObjectType)
updateFacet_objectType = Lens.lens (\UpdateFacet' {objectType} -> objectType) (\s@UpdateFacet' {} a -> s {objectType = a} :: UpdateFacet)

-- | The Amazon Resource Name (ARN) that is associated with the Facet. For
-- more information, see arns.
updateFacet_schemaArn :: Lens.Lens' UpdateFacet Core.Text
updateFacet_schemaArn = Lens.lens (\UpdateFacet' {schemaArn} -> schemaArn) (\s@UpdateFacet' {} a -> s {schemaArn = a} :: UpdateFacet)

-- | The name of the facet.
updateFacet_name :: Lens.Lens' UpdateFacet Core.Text
updateFacet_name = Lens.lens (\UpdateFacet' {name} -> name) (\s@UpdateFacet' {} a -> s {name = a} :: UpdateFacet)

instance Core.AWSRequest UpdateFacet where
  type AWSResponse UpdateFacet = UpdateFacetResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateFacetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateFacet

instance Core.NFData UpdateFacet

instance Core.ToHeaders UpdateFacet where
  toHeaders UpdateFacet' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# schemaArn]

instance Core.ToJSON UpdateFacet where
  toJSON UpdateFacet' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AttributeUpdates" Core..=)
              Core.<$> attributeUpdates,
            ("ObjectType" Core..=) Core.<$> objectType,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath UpdateFacet where
  toPath =
    Core.const "/amazonclouddirectory/2017-01-11/facet"

instance Core.ToQuery UpdateFacet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateFacetResponse' smart constructor.
data UpdateFacetResponse = UpdateFacetResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateFacetResponse
newUpdateFacetResponse pHttpStatus_ =
  UpdateFacetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateFacetResponse_httpStatus :: Lens.Lens' UpdateFacetResponse Core.Int
updateFacetResponse_httpStatus = Lens.lens (\UpdateFacetResponse' {httpStatus} -> httpStatus) (\s@UpdateFacetResponse' {} a -> s {httpStatus = a} :: UpdateFacetResponse)

instance Core.NFData UpdateFacetResponse
