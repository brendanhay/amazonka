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
-- Module      : Amazonka.DirectoryService.DeleteLogSubscription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified log subscription.
module Amazonka.DirectoryService.DeleteLogSubscription
  ( -- * Creating a Request
    DeleteLogSubscription (..),
    newDeleteLogSubscription,

    -- * Request Lenses
    deleteLogSubscription_directoryId,

    -- * Destructuring the Response
    DeleteLogSubscriptionResponse (..),
    newDeleteLogSubscriptionResponse,

    -- * Response Lenses
    deleteLogSubscriptionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLogSubscription' smart constructor.
data DeleteLogSubscription = DeleteLogSubscription'
  { -- | Identifier of the directory whose log subscription you want to delete.
    directoryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLogSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'deleteLogSubscription_directoryId' - Identifier of the directory whose log subscription you want to delete.
newDeleteLogSubscription ::
  -- | 'directoryId'
  Prelude.Text ->
  DeleteLogSubscription
newDeleteLogSubscription pDirectoryId_ =
  DeleteLogSubscription' {directoryId = pDirectoryId_}

-- | Identifier of the directory whose log subscription you want to delete.
deleteLogSubscription_directoryId :: Lens.Lens' DeleteLogSubscription Prelude.Text
deleteLogSubscription_directoryId = Lens.lens (\DeleteLogSubscription' {directoryId} -> directoryId) (\s@DeleteLogSubscription' {} a -> s {directoryId = a} :: DeleteLogSubscription)

instance Core.AWSRequest DeleteLogSubscription where
  type
    AWSResponse DeleteLogSubscription =
      DeleteLogSubscriptionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLogSubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLogSubscription where
  hashWithSalt _salt DeleteLogSubscription' {..} =
    _salt `Prelude.hashWithSalt` directoryId

instance Prelude.NFData DeleteLogSubscription where
  rnf DeleteLogSubscription' {..} =
    Prelude.rnf directoryId

instance Core.ToHeaders DeleteLogSubscription where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.DeleteLogSubscription" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteLogSubscription where
  toJSON DeleteLogSubscription' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("DirectoryId" Core..= directoryId)]
      )

instance Core.ToPath DeleteLogSubscription where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteLogSubscription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLogSubscriptionResponse' smart constructor.
data DeleteLogSubscriptionResponse = DeleteLogSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLogSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteLogSubscriptionResponse_httpStatus' - The response's http status code.
newDeleteLogSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLogSubscriptionResponse
newDeleteLogSubscriptionResponse pHttpStatus_ =
  DeleteLogSubscriptionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteLogSubscriptionResponse_httpStatus :: Lens.Lens' DeleteLogSubscriptionResponse Prelude.Int
deleteLogSubscriptionResponse_httpStatus = Lens.lens (\DeleteLogSubscriptionResponse' {httpStatus} -> httpStatus) (\s@DeleteLogSubscriptionResponse' {} a -> s {httpStatus = a} :: DeleteLogSubscriptionResponse)

instance Prelude.NFData DeleteLogSubscriptionResponse where
  rnf DeleteLogSubscriptionResponse' {..} =
    Prelude.rnf httpStatus
