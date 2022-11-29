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
-- Module      : Amazonka.Config.DeleteStoredQuery
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the stored query for a single Amazon Web Services account and a
-- single Amazon Web Services Region.
module Amazonka.Config.DeleteStoredQuery
  ( -- * Creating a Request
    DeleteStoredQuery (..),
    newDeleteStoredQuery,

    -- * Request Lenses
    deleteStoredQuery_queryName,

    -- * Destructuring the Response
    DeleteStoredQueryResponse (..),
    newDeleteStoredQueryResponse,

    -- * Response Lenses
    deleteStoredQueryResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteStoredQuery' smart constructor.
data DeleteStoredQuery = DeleteStoredQuery'
  { -- | The name of the query that you want to delete.
    queryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStoredQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryName', 'deleteStoredQuery_queryName' - The name of the query that you want to delete.
newDeleteStoredQuery ::
  -- | 'queryName'
  Prelude.Text ->
  DeleteStoredQuery
newDeleteStoredQuery pQueryName_ =
  DeleteStoredQuery' {queryName = pQueryName_}

-- | The name of the query that you want to delete.
deleteStoredQuery_queryName :: Lens.Lens' DeleteStoredQuery Prelude.Text
deleteStoredQuery_queryName = Lens.lens (\DeleteStoredQuery' {queryName} -> queryName) (\s@DeleteStoredQuery' {} a -> s {queryName = a} :: DeleteStoredQuery)

instance Core.AWSRequest DeleteStoredQuery where
  type
    AWSResponse DeleteStoredQuery =
      DeleteStoredQueryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteStoredQueryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteStoredQuery where
  hashWithSalt _salt DeleteStoredQuery' {..} =
    _salt `Prelude.hashWithSalt` queryName

instance Prelude.NFData DeleteStoredQuery where
  rnf DeleteStoredQuery' {..} = Prelude.rnf queryName

instance Core.ToHeaders DeleteStoredQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DeleteStoredQuery" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteStoredQuery where
  toJSON DeleteStoredQuery' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("QueryName" Core..= queryName)]
      )

instance Core.ToPath DeleteStoredQuery where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteStoredQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStoredQueryResponse' smart constructor.
data DeleteStoredQueryResponse = DeleteStoredQueryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStoredQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteStoredQueryResponse_httpStatus' - The response's http status code.
newDeleteStoredQueryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteStoredQueryResponse
newDeleteStoredQueryResponse pHttpStatus_ =
  DeleteStoredQueryResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteStoredQueryResponse_httpStatus :: Lens.Lens' DeleteStoredQueryResponse Prelude.Int
deleteStoredQueryResponse_httpStatus = Lens.lens (\DeleteStoredQueryResponse' {httpStatus} -> httpStatus) (\s@DeleteStoredQueryResponse' {} a -> s {httpStatus = a} :: DeleteStoredQueryResponse)

instance Prelude.NFData DeleteStoredQueryResponse where
  rnf DeleteStoredQueryResponse' {..} =
    Prelude.rnf httpStatus
