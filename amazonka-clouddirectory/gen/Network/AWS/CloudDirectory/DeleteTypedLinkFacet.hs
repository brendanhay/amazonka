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
-- Module      : Network.AWS.CloudDirectory.DeleteTypedLinkFacet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a TypedLinkFacet. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
module Network.AWS.CloudDirectory.DeleteTypedLinkFacet
  ( -- * Creating a Request
    DeleteTypedLinkFacet (..),
    newDeleteTypedLinkFacet,

    -- * Request Lenses
    deleteTypedLinkFacet_schemaArn,
    deleteTypedLinkFacet_name,

    -- * Destructuring the Response
    DeleteTypedLinkFacetResponse (..),
    newDeleteTypedLinkFacetResponse,

    -- * Response Lenses
    deleteTypedLinkFacetResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteTypedLinkFacet' smart constructor.
data DeleteTypedLinkFacet = DeleteTypedLinkFacet'
  { -- | The Amazon Resource Name (ARN) that is associated with the schema. For
    -- more information, see arns.
    schemaArn :: Prelude.Text,
    -- | The unique name of the typed link facet.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTypedLinkFacet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'deleteTypedLinkFacet_schemaArn' - The Amazon Resource Name (ARN) that is associated with the schema. For
-- more information, see arns.
--
-- 'name', 'deleteTypedLinkFacet_name' - The unique name of the typed link facet.
newDeleteTypedLinkFacet ::
  -- | 'schemaArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  DeleteTypedLinkFacet
newDeleteTypedLinkFacet pSchemaArn_ pName_ =
  DeleteTypedLinkFacet'
    { schemaArn = pSchemaArn_,
      name = pName_
    }

-- | The Amazon Resource Name (ARN) that is associated with the schema. For
-- more information, see arns.
deleteTypedLinkFacet_schemaArn :: Lens.Lens' DeleteTypedLinkFacet Prelude.Text
deleteTypedLinkFacet_schemaArn = Lens.lens (\DeleteTypedLinkFacet' {schemaArn} -> schemaArn) (\s@DeleteTypedLinkFacet' {} a -> s {schemaArn = a} :: DeleteTypedLinkFacet)

-- | The unique name of the typed link facet.
deleteTypedLinkFacet_name :: Lens.Lens' DeleteTypedLinkFacet Prelude.Text
deleteTypedLinkFacet_name = Lens.lens (\DeleteTypedLinkFacet' {name} -> name) (\s@DeleteTypedLinkFacet' {} a -> s {name = a} :: DeleteTypedLinkFacet)

instance Prelude.AWSRequest DeleteTypedLinkFacet where
  type
    Rs DeleteTypedLinkFacet =
      DeleteTypedLinkFacetResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTypedLinkFacetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTypedLinkFacet

instance Prelude.NFData DeleteTypedLinkFacet

instance Prelude.ToHeaders DeleteTypedLinkFacet where
  toHeaders DeleteTypedLinkFacet' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Prelude.=# schemaArn]

instance Prelude.ToJSON DeleteTypedLinkFacet where
  toJSON DeleteTypedLinkFacet' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath DeleteTypedLinkFacet where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/typedlink/facet/delete"

instance Prelude.ToQuery DeleteTypedLinkFacet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTypedLinkFacetResponse' smart constructor.
data DeleteTypedLinkFacetResponse = DeleteTypedLinkFacetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTypedLinkFacetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteTypedLinkFacetResponse_httpStatus' - The response's http status code.
newDeleteTypedLinkFacetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTypedLinkFacetResponse
newDeleteTypedLinkFacetResponse pHttpStatus_ =
  DeleteTypedLinkFacetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteTypedLinkFacetResponse_httpStatus :: Lens.Lens' DeleteTypedLinkFacetResponse Prelude.Int
deleteTypedLinkFacetResponse_httpStatus = Lens.lens (\DeleteTypedLinkFacetResponse' {httpStatus} -> httpStatus) (\s@DeleteTypedLinkFacetResponse' {} a -> s {httpStatus = a} :: DeleteTypedLinkFacetResponse)

instance Prelude.NFData DeleteTypedLinkFacetResponse
