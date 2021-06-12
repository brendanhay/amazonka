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
-- Module      : Network.AWS.Athena.DeleteNamedQuery
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the named query if you have access to the workgroup in which the
-- query was saved.
--
-- For code samples using the AWS SDK for Java, see
-- <http://docs.aws.amazon.com/athena/latest/ug/code-samples.html Examples and Code Samples>
-- in the /Amazon Athena User Guide/.
module Network.AWS.Athena.DeleteNamedQuery
  ( -- * Creating a Request
    DeleteNamedQuery (..),
    newDeleteNamedQuery,

    -- * Request Lenses
    deleteNamedQuery_namedQueryId,

    -- * Destructuring the Response
    DeleteNamedQueryResponse (..),
    newDeleteNamedQueryResponse,

    -- * Response Lenses
    deleteNamedQueryResponse_httpStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteNamedQuery' smart constructor.
data DeleteNamedQuery = DeleteNamedQuery'
  { -- | The unique ID of the query to delete.
    namedQueryId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteNamedQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namedQueryId', 'deleteNamedQuery_namedQueryId' - The unique ID of the query to delete.
newDeleteNamedQuery ::
  -- | 'namedQueryId'
  Core.Text ->
  DeleteNamedQuery
newDeleteNamedQuery pNamedQueryId_ =
  DeleteNamedQuery' {namedQueryId = pNamedQueryId_}

-- | The unique ID of the query to delete.
deleteNamedQuery_namedQueryId :: Lens.Lens' DeleteNamedQuery Core.Text
deleteNamedQuery_namedQueryId = Lens.lens (\DeleteNamedQuery' {namedQueryId} -> namedQueryId) (\s@DeleteNamedQuery' {} a -> s {namedQueryId = a} :: DeleteNamedQuery)

instance Core.AWSRequest DeleteNamedQuery where
  type
    AWSResponse DeleteNamedQuery =
      DeleteNamedQueryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteNamedQueryResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteNamedQuery

instance Core.NFData DeleteNamedQuery

instance Core.ToHeaders DeleteNamedQuery where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonAthena.DeleteNamedQuery" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteNamedQuery where
  toJSON DeleteNamedQuery' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("NamedQueryId" Core..= namedQueryId)]
      )

instance Core.ToPath DeleteNamedQuery where
  toPath = Core.const "/"

instance Core.ToQuery DeleteNamedQuery where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteNamedQueryResponse' smart constructor.
data DeleteNamedQueryResponse = DeleteNamedQueryResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteNamedQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteNamedQueryResponse_httpStatus' - The response's http status code.
newDeleteNamedQueryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteNamedQueryResponse
newDeleteNamedQueryResponse pHttpStatus_ =
  DeleteNamedQueryResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteNamedQueryResponse_httpStatus :: Lens.Lens' DeleteNamedQueryResponse Core.Int
deleteNamedQueryResponse_httpStatus = Lens.lens (\DeleteNamedQueryResponse' {httpStatus} -> httpStatus) (\s@DeleteNamedQueryResponse' {} a -> s {httpStatus = a} :: DeleteNamedQueryResponse)

instance Core.NFData DeleteNamedQueryResponse
