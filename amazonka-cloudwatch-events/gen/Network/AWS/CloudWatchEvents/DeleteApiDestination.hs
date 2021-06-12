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
-- Module      : Network.AWS.CloudWatchEvents.DeleteApiDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified API destination.
module Network.AWS.CloudWatchEvents.DeleteApiDestination
  ( -- * Creating a Request
    DeleteApiDestination (..),
    newDeleteApiDestination,

    -- * Request Lenses
    deleteApiDestination_name,

    -- * Destructuring the Response
    DeleteApiDestinationResponse (..),
    newDeleteApiDestinationResponse,

    -- * Response Lenses
    deleteApiDestinationResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteApiDestination' smart constructor.
data DeleteApiDestination = DeleteApiDestination'
  { -- | The name of the destination to delete.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteApiDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteApiDestination_name' - The name of the destination to delete.
newDeleteApiDestination ::
  -- | 'name'
  Core.Text ->
  DeleteApiDestination
newDeleteApiDestination pName_ =
  DeleteApiDestination' {name = pName_}

-- | The name of the destination to delete.
deleteApiDestination_name :: Lens.Lens' DeleteApiDestination Core.Text
deleteApiDestination_name = Lens.lens (\DeleteApiDestination' {name} -> name) (\s@DeleteApiDestination' {} a -> s {name = a} :: DeleteApiDestination)

instance Core.AWSRequest DeleteApiDestination where
  type
    AWSResponse DeleteApiDestination =
      DeleteApiDestinationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteApiDestinationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteApiDestination

instance Core.NFData DeleteApiDestination

instance Core.ToHeaders DeleteApiDestination where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSEvents.DeleteApiDestination" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteApiDestination where
  toJSON DeleteApiDestination' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath DeleteApiDestination where
  toPath = Core.const "/"

instance Core.ToQuery DeleteApiDestination where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteApiDestinationResponse' smart constructor.
data DeleteApiDestinationResponse = DeleteApiDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteApiDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteApiDestinationResponse_httpStatus' - The response's http status code.
newDeleteApiDestinationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteApiDestinationResponse
newDeleteApiDestinationResponse pHttpStatus_ =
  DeleteApiDestinationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteApiDestinationResponse_httpStatus :: Lens.Lens' DeleteApiDestinationResponse Core.Int
deleteApiDestinationResponse_httpStatus = Lens.lens (\DeleteApiDestinationResponse' {httpStatus} -> httpStatus) (\s@DeleteApiDestinationResponse' {} a -> s {httpStatus = a} :: DeleteApiDestinationResponse)

instance Core.NFData DeleteApiDestinationResponse
