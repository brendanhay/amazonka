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
-- Module      : Amazonka.Athena.DeleteNamedQuery
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the named query if you have access to the workgroup in which the
-- query was saved.
--
-- For code samples using the Amazon Web Services SDK for Java, see
-- <http://docs.aws.amazon.com/athena/latest/ug/code-samples.html Examples and Code Samples>
-- in the /Amazon Athena User Guide/.
module Amazonka.Athena.DeleteNamedQuery
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

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteNamedQuery' smart constructor.
data DeleteNamedQuery = DeleteNamedQuery'
  { -- | The unique ID of the query to delete.
    namedQueryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteNamedQuery
newDeleteNamedQuery pNamedQueryId_ =
  DeleteNamedQuery' {namedQueryId = pNamedQueryId_}

-- | The unique ID of the query to delete.
deleteNamedQuery_namedQueryId :: Lens.Lens' DeleteNamedQuery Prelude.Text
deleteNamedQuery_namedQueryId = Lens.lens (\DeleteNamedQuery' {namedQueryId} -> namedQueryId) (\s@DeleteNamedQuery' {} a -> s {namedQueryId = a} :: DeleteNamedQuery)

instance Core.AWSRequest DeleteNamedQuery where
  type
    AWSResponse DeleteNamedQuery =
      DeleteNamedQueryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteNamedQueryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteNamedQuery where
  hashWithSalt _salt DeleteNamedQuery' {..} =
    _salt `Prelude.hashWithSalt` namedQueryId

instance Prelude.NFData DeleteNamedQuery where
  rnf DeleteNamedQuery' {..} = Prelude.rnf namedQueryId

instance Data.ToHeaders DeleteNamedQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.DeleteNamedQuery" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteNamedQuery where
  toJSON DeleteNamedQuery' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("NamedQueryId" Data..= namedQueryId)]
      )

instance Data.ToPath DeleteNamedQuery where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteNamedQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteNamedQueryResponse' smart constructor.
data DeleteNamedQueryResponse = DeleteNamedQueryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteNamedQueryResponse
newDeleteNamedQueryResponse pHttpStatus_ =
  DeleteNamedQueryResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteNamedQueryResponse_httpStatus :: Lens.Lens' DeleteNamedQueryResponse Prelude.Int
deleteNamedQueryResponse_httpStatus = Lens.lens (\DeleteNamedQueryResponse' {httpStatus} -> httpStatus) (\s@DeleteNamedQueryResponse' {} a -> s {httpStatus = a} :: DeleteNamedQueryResponse)

instance Prelude.NFData DeleteNamedQueryResponse where
  rnf DeleteNamedQueryResponse' {..} =
    Prelude.rnf httpStatus
