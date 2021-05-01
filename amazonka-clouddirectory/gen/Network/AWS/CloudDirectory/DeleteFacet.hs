{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteFacet' smart constructor.
data DeleteFacet = DeleteFacet'
  { -- | The Amazon Resource Name (ARN) that is associated with the Facet. For
    -- more information, see arns.
    schemaArn :: Prelude.Text,
    -- | The name of the facet to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  DeleteFacet
newDeleteFacet pSchemaArn_ pName_ =
  DeleteFacet'
    { schemaArn = pSchemaArn_,
      name = pName_
    }

-- | The Amazon Resource Name (ARN) that is associated with the Facet. For
-- more information, see arns.
deleteFacet_schemaArn :: Lens.Lens' DeleteFacet Prelude.Text
deleteFacet_schemaArn = Lens.lens (\DeleteFacet' {schemaArn} -> schemaArn) (\s@DeleteFacet' {} a -> s {schemaArn = a} :: DeleteFacet)

-- | The name of the facet to delete.
deleteFacet_name :: Lens.Lens' DeleteFacet Prelude.Text
deleteFacet_name = Lens.lens (\DeleteFacet' {name} -> name) (\s@DeleteFacet' {} a -> s {name = a} :: DeleteFacet)

instance Prelude.AWSRequest DeleteFacet where
  type Rs DeleteFacet = DeleteFacetResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteFacetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFacet

instance Prelude.NFData DeleteFacet

instance Prelude.ToHeaders DeleteFacet where
  toHeaders DeleteFacet' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Prelude.=# schemaArn]

instance Prelude.ToJSON DeleteFacet where
  toJSON DeleteFacet' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath DeleteFacet where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/facet/delete"

instance Prelude.ToQuery DeleteFacet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFacetResponse' smart constructor.
data DeleteFacetResponse = DeleteFacetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteFacetResponse
newDeleteFacetResponse pHttpStatus_ =
  DeleteFacetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteFacetResponse_httpStatus :: Lens.Lens' DeleteFacetResponse Prelude.Int
deleteFacetResponse_httpStatus = Lens.lens (\DeleteFacetResponse' {httpStatus} -> httpStatus) (\s@DeleteFacetResponse' {} a -> s {httpStatus = a} :: DeleteFacetResponse)

instance Prelude.NFData DeleteFacetResponse
