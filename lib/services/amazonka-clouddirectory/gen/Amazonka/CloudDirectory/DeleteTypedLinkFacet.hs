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
-- Module      : Amazonka.CloudDirectory.DeleteTypedLinkFacet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a TypedLinkFacet. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
module Amazonka.CloudDirectory.DeleteTypedLinkFacet
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

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTypedLinkFacet' smart constructor.
data DeleteTypedLinkFacet = DeleteTypedLinkFacet'
  { -- | The Amazon Resource Name (ARN) that is associated with the schema. For
    -- more information, see arns.
    schemaArn :: Prelude.Text,
    -- | The unique name of the typed link facet.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteTypedLinkFacet where
  type
    AWSResponse DeleteTypedLinkFacet =
      DeleteTypedLinkFacetResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTypedLinkFacetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTypedLinkFacet where
  hashWithSalt _salt DeleteTypedLinkFacet' {..} =
    _salt
      `Prelude.hashWithSalt` schemaArn
      `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteTypedLinkFacet where
  rnf DeleteTypedLinkFacet' {..} =
    Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders DeleteTypedLinkFacet where
  toHeaders DeleteTypedLinkFacet' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Data.=# schemaArn]

instance Data.ToJSON DeleteTypedLinkFacet where
  toJSON DeleteTypedLinkFacet' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DeleteTypedLinkFacet where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/typedlink/facet/delete"

instance Data.ToQuery DeleteTypedLinkFacet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTypedLinkFacetResponse' smart constructor.
data DeleteTypedLinkFacetResponse = DeleteTypedLinkFacetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DeleteTypedLinkFacetResponse where
  rnf DeleteTypedLinkFacetResponse' {..} =
    Prelude.rnf httpStatus
