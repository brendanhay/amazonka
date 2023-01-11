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
-- Module      : Amazonka.Route53.DeleteCidrCollection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a CIDR collection in the current Amazon Web Services account.
-- The collection must be empty before it can be deleted.
module Amazonka.Route53.DeleteCidrCollection
  ( -- * Creating a Request
    DeleteCidrCollection (..),
    newDeleteCidrCollection,

    -- * Request Lenses
    deleteCidrCollection_id,

    -- * Destructuring the Response
    DeleteCidrCollectionResponse (..),
    newDeleteCidrCollectionResponse,

    -- * Response Lenses
    deleteCidrCollectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | /See:/ 'newDeleteCidrCollection' smart constructor.
data DeleteCidrCollection = DeleteCidrCollection'
  { -- | The UUID of the collection to delete.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCidrCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteCidrCollection_id' - The UUID of the collection to delete.
newDeleteCidrCollection ::
  -- | 'id'
  Prelude.Text ->
  DeleteCidrCollection
newDeleteCidrCollection pId_ =
  DeleteCidrCollection' {id = pId_}

-- | The UUID of the collection to delete.
deleteCidrCollection_id :: Lens.Lens' DeleteCidrCollection Prelude.Text
deleteCidrCollection_id = Lens.lens (\DeleteCidrCollection' {id} -> id) (\s@DeleteCidrCollection' {} a -> s {id = a} :: DeleteCidrCollection)

instance Core.AWSRequest DeleteCidrCollection where
  type
    AWSResponse DeleteCidrCollection =
      DeleteCidrCollectionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCidrCollectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCidrCollection where
  hashWithSalt _salt DeleteCidrCollection' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteCidrCollection where
  rnf DeleteCidrCollection' {..} = Prelude.rnf id

instance Data.ToHeaders DeleteCidrCollection where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteCidrCollection where
  toPath DeleteCidrCollection' {..} =
    Prelude.mconcat
      ["/2013-04-01/cidrcollection/", Data.toBS id]

instance Data.ToQuery DeleteCidrCollection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCidrCollectionResponse' smart constructor.
data DeleteCidrCollectionResponse = DeleteCidrCollectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCidrCollectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteCidrCollectionResponse_httpStatus' - The response's http status code.
newDeleteCidrCollectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCidrCollectionResponse
newDeleteCidrCollectionResponse pHttpStatus_ =
  DeleteCidrCollectionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteCidrCollectionResponse_httpStatus :: Lens.Lens' DeleteCidrCollectionResponse Prelude.Int
deleteCidrCollectionResponse_httpStatus = Lens.lens (\DeleteCidrCollectionResponse' {httpStatus} -> httpStatus) (\s@DeleteCidrCollectionResponse' {} a -> s {httpStatus = a} :: DeleteCidrCollectionResponse)

instance Prelude.NFData DeleteCidrCollectionResponse where
  rnf DeleteCidrCollectionResponse' {..} =
    Prelude.rnf httpStatus
