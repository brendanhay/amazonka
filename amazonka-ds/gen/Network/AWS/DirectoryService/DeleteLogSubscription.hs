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
-- Module      : Network.AWS.DirectoryService.DeleteLogSubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified log subscription.
module Network.AWS.DirectoryService.DeleteLogSubscription
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

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLogSubscription' smart constructor.
data DeleteLogSubscription = DeleteLogSubscription'
  { -- | Identifier of the directory whose log subscription you want to delete.
    directoryId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteLogSubscription
newDeleteLogSubscription pDirectoryId_ =
  DeleteLogSubscription' {directoryId = pDirectoryId_}

-- | Identifier of the directory whose log subscription you want to delete.
deleteLogSubscription_directoryId :: Lens.Lens' DeleteLogSubscription Core.Text
deleteLogSubscription_directoryId = Lens.lens (\DeleteLogSubscription' {directoryId} -> directoryId) (\s@DeleteLogSubscription' {} a -> s {directoryId = a} :: DeleteLogSubscription)

instance Core.AWSRequest DeleteLogSubscription where
  type
    AWSResponse DeleteLogSubscription =
      DeleteLogSubscriptionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLogSubscriptionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteLogSubscription

instance Core.NFData DeleteLogSubscription

instance Core.ToHeaders DeleteLogSubscription where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.DeleteLogSubscription" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteLogSubscription where
  toJSON DeleteLogSubscription' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("DirectoryId" Core..= directoryId)]
      )

instance Core.ToPath DeleteLogSubscription where
  toPath = Core.const "/"

instance Core.ToQuery DeleteLogSubscription where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteLogSubscriptionResponse' smart constructor.
data DeleteLogSubscriptionResponse = DeleteLogSubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteLogSubscriptionResponse
newDeleteLogSubscriptionResponse pHttpStatus_ =
  DeleteLogSubscriptionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteLogSubscriptionResponse_httpStatus :: Lens.Lens' DeleteLogSubscriptionResponse Core.Int
deleteLogSubscriptionResponse_httpStatus = Lens.lens (\DeleteLogSubscriptionResponse' {httpStatus} -> httpStatus) (\s@DeleteLogSubscriptionResponse' {} a -> s {httpStatus = a} :: DeleteLogSubscriptionResponse)

instance Core.NFData DeleteLogSubscriptionResponse
