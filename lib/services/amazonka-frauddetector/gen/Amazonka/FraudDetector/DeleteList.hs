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
-- Module      : Amazonka.FraudDetector.DeleteList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the list, provided it is not used in a rule.
--
-- When you delete a list, Amazon Fraud Detector permanently deletes that
-- list and the elements in the list.
module Amazonka.FraudDetector.DeleteList
  ( -- * Creating a Request
    DeleteList (..),
    newDeleteList,

    -- * Request Lenses
    deleteList_name,

    -- * Destructuring the Response
    DeleteListResponse (..),
    newDeleteListResponse,

    -- * Response Lenses
    deleteListResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteList' smart constructor.
data DeleteList = DeleteList'
  { -- | The name of the list to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteList_name' - The name of the list to delete.
newDeleteList ::
  -- | 'name'
  Prelude.Text ->
  DeleteList
newDeleteList pName_ = DeleteList' {name = pName_}

-- | The name of the list to delete.
deleteList_name :: Lens.Lens' DeleteList Prelude.Text
deleteList_name = Lens.lens (\DeleteList' {name} -> name) (\s@DeleteList' {} a -> s {name = a} :: DeleteList)

instance Core.AWSRequest DeleteList where
  type AWSResponse DeleteList = DeleteListResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteListResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteList where
  hashWithSalt _salt DeleteList' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteList where
  rnf DeleteList' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteList where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.DeleteList" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteList where
  toJSON DeleteList' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Data..= name)]
      )

instance Data.ToPath DeleteList where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteList where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteListResponse' smart constructor.
data DeleteListResponse = DeleteListResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteListResponse_httpStatus' - The response's http status code.
newDeleteListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteListResponse
newDeleteListResponse pHttpStatus_ =
  DeleteListResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteListResponse_httpStatus :: Lens.Lens' DeleteListResponse Prelude.Int
deleteListResponse_httpStatus = Lens.lens (\DeleteListResponse' {httpStatus} -> httpStatus) (\s@DeleteListResponse' {} a -> s {httpStatus = a} :: DeleteListResponse)

instance Prelude.NFData DeleteListResponse where
  rnf DeleteListResponse' {..} = Prelude.rnf httpStatus
