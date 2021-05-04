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
-- Module      : Network.AWS.Config.DeleteStoredQuery
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the stored query for a single AWS account and a single AWS
-- Region.
module Network.AWS.Config.DeleteStoredQuery
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

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteStoredQuery' smart constructor.
data DeleteStoredQuery = DeleteStoredQuery'
  { -- | The name of the query that you want to delete.
    queryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteStoredQuery where
  type Rs DeleteStoredQuery = DeleteStoredQueryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteStoredQueryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteStoredQuery

instance Prelude.NFData DeleteStoredQuery

instance Prelude.ToHeaders DeleteStoredQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StarlingDoveService.DeleteStoredQuery" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteStoredQuery where
  toJSON DeleteStoredQuery' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("QueryName" Prelude..= queryName)]
      )

instance Prelude.ToPath DeleteStoredQuery where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteStoredQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStoredQueryResponse' smart constructor.
data DeleteStoredQueryResponse = DeleteStoredQueryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteStoredQueryResponse
