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
-- Module      : Amazonka.MacieV2.DeleteAllowList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an allow list.
module Amazonka.MacieV2.DeleteAllowList
  ( -- * Creating a Request
    DeleteAllowList (..),
    newDeleteAllowList,

    -- * Request Lenses
    deleteAllowList_ignoreJobChecks,
    deleteAllowList_id,

    -- * Destructuring the Response
    DeleteAllowListResponse (..),
    newDeleteAllowListResponse,

    -- * Response Lenses
    deleteAllowListResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAllowList' smart constructor.
data DeleteAllowList = DeleteAllowList'
  { -- | Specifies whether to force deletion of the allow list, even if active
    -- classification jobs are configured to use the list.
    --
    -- When you try to delete an allow list, Amazon Macie checks for
    -- classification jobs that use the list and have a status other than
    -- COMPLETE or CANCELLED. By default, Macie rejects your request if any
    -- jobs meet these criteria. To skip these checks and delete the list, set
    -- this value to true. To delete the list only if no active jobs are
    -- configured to use it, set this value to false.
    ignoreJobChecks :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the Amazon Macie resource that the request
    -- applies to.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAllowList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ignoreJobChecks', 'deleteAllowList_ignoreJobChecks' - Specifies whether to force deletion of the allow list, even if active
-- classification jobs are configured to use the list.
--
-- When you try to delete an allow list, Amazon Macie checks for
-- classification jobs that use the list and have a status other than
-- COMPLETE or CANCELLED. By default, Macie rejects your request if any
-- jobs meet these criteria. To skip these checks and delete the list, set
-- this value to true. To delete the list only if no active jobs are
-- configured to use it, set this value to false.
--
-- 'id', 'deleteAllowList_id' - The unique identifier for the Amazon Macie resource that the request
-- applies to.
newDeleteAllowList ::
  -- | 'id'
  Prelude.Text ->
  DeleteAllowList
newDeleteAllowList pId_ =
  DeleteAllowList'
    { ignoreJobChecks = Prelude.Nothing,
      id = pId_
    }

-- | Specifies whether to force deletion of the allow list, even if active
-- classification jobs are configured to use the list.
--
-- When you try to delete an allow list, Amazon Macie checks for
-- classification jobs that use the list and have a status other than
-- COMPLETE or CANCELLED. By default, Macie rejects your request if any
-- jobs meet these criteria. To skip these checks and delete the list, set
-- this value to true. To delete the list only if no active jobs are
-- configured to use it, set this value to false.
deleteAllowList_ignoreJobChecks :: Lens.Lens' DeleteAllowList (Prelude.Maybe Prelude.Text)
deleteAllowList_ignoreJobChecks = Lens.lens (\DeleteAllowList' {ignoreJobChecks} -> ignoreJobChecks) (\s@DeleteAllowList' {} a -> s {ignoreJobChecks = a} :: DeleteAllowList)

-- | The unique identifier for the Amazon Macie resource that the request
-- applies to.
deleteAllowList_id :: Lens.Lens' DeleteAllowList Prelude.Text
deleteAllowList_id = Lens.lens (\DeleteAllowList' {id} -> id) (\s@DeleteAllowList' {} a -> s {id = a} :: DeleteAllowList)

instance Core.AWSRequest DeleteAllowList where
  type
    AWSResponse DeleteAllowList =
      DeleteAllowListResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAllowListResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAllowList where
  hashWithSalt _salt DeleteAllowList' {..} =
    _salt
      `Prelude.hashWithSalt` ignoreJobChecks
      `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteAllowList where
  rnf DeleteAllowList' {..} =
    Prelude.rnf ignoreJobChecks
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders DeleteAllowList where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteAllowList where
  toPath DeleteAllowList' {..} =
    Prelude.mconcat ["/allow-lists/", Data.toBS id]

instance Data.ToQuery DeleteAllowList where
  toQuery DeleteAllowList' {..} =
    Prelude.mconcat
      ["ignoreJobChecks" Data.=: ignoreJobChecks]

-- | /See:/ 'newDeleteAllowListResponse' smart constructor.
data DeleteAllowListResponse = DeleteAllowListResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAllowListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAllowListResponse_httpStatus' - The response's http status code.
newDeleteAllowListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAllowListResponse
newDeleteAllowListResponse pHttpStatus_ =
  DeleteAllowListResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteAllowListResponse_httpStatus :: Lens.Lens' DeleteAllowListResponse Prelude.Int
deleteAllowListResponse_httpStatus = Lens.lens (\DeleteAllowListResponse' {httpStatus} -> httpStatus) (\s@DeleteAllowListResponse' {} a -> s {httpStatus = a} :: DeleteAllowListResponse)

instance Prelude.NFData DeleteAllowListResponse where
  rnf DeleteAllowListResponse' {..} =
    Prelude.rnf httpStatus
