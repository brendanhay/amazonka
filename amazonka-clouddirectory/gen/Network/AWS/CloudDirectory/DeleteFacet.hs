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
-- Module      : Network.AWS.CloudDirectory.DeleteFacet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a given Facet. All attributes and Rules that are associated with
-- the facet will be deleted. Only development schema facets are allowed
-- deletion.
module Network.AWS.CloudDirectory.DeleteFacet
  ( -- * Creating a Request
    DeleteFacet (..),
    newDeleteFacet,

    -- * Request Lenses
    deleteFacet_schemaArn,
    deleteFacet_name,

    -- * Destructuring the Response
    DeleteFacetResponse (..),
    newDeleteFacetResponse,

    -- * Response Lenses
    deleteFacetResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteFacet' smart constructor.
data DeleteFacet = DeleteFacet'
  { -- | The Amazon Resource Name (ARN) that is associated with the Facet. For
    -- more information, see arns.
    schemaArn :: Core.Text,
    -- | The name of the facet to delete.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteFacet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'deleteFacet_schemaArn' - The Amazon Resource Name (ARN) that is associated with the Facet. For
-- more information, see arns.
--
-- 'name', 'deleteFacet_name' - The name of the facet to delete.
newDeleteFacet ::
  -- | 'schemaArn'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  DeleteFacet
newDeleteFacet pSchemaArn_ pName_ =
  DeleteFacet'
    { schemaArn = pSchemaArn_,
      name = pName_
    }

-- | The Amazon Resource Name (ARN) that is associated with the Facet. For
-- more information, see arns.
deleteFacet_schemaArn :: Lens.Lens' DeleteFacet Core.Text
deleteFacet_schemaArn = Lens.lens (\DeleteFacet' {schemaArn} -> schemaArn) (\s@DeleteFacet' {} a -> s {schemaArn = a} :: DeleteFacet)

-- | The name of the facet to delete.
deleteFacet_name :: Lens.Lens' DeleteFacet Core.Text
deleteFacet_name = Lens.lens (\DeleteFacet' {name} -> name) (\s@DeleteFacet' {} a -> s {name = a} :: DeleteFacet)

instance Core.AWSRequest DeleteFacet where
  type AWSResponse DeleteFacet = DeleteFacetResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteFacetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteFacet

instance Core.NFData DeleteFacet

instance Core.ToHeaders DeleteFacet where
  toHeaders DeleteFacet' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# schemaArn]

instance Core.ToJSON DeleteFacet where
  toJSON DeleteFacet' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath DeleteFacet where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/facet/delete"

instance Core.ToQuery DeleteFacet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteFacetResponse' smart constructor.
data DeleteFacetResponse = DeleteFacetResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteFacetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteFacetResponse_httpStatus' - The response's http status code.
newDeleteFacetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteFacetResponse
newDeleteFacetResponse pHttpStatus_ =
  DeleteFacetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteFacetResponse_httpStatus :: Lens.Lens' DeleteFacetResponse Core.Int
deleteFacetResponse_httpStatus = Lens.lens (\DeleteFacetResponse' {httpStatus} -> httpStatus) (\s@DeleteFacetResponse' {} a -> s {httpStatus = a} :: DeleteFacetResponse)

instance Core.NFData DeleteFacetResponse
