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
-- Module      : Network.AWS.MediaLive.DeleteSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete all schedule actions on a channel.
module Network.AWS.MediaLive.DeleteSchedule
  ( -- * Creating a Request
    DeleteSchedule (..),
    newDeleteSchedule,

    -- * Request Lenses
    deleteSchedule_channelId,

    -- * Destructuring the Response
    DeleteScheduleResponse (..),
    newDeleteScheduleResponse,

    -- * Response Lenses
    deleteScheduleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DeleteScheduleRequest
--
-- /See:/ 'newDeleteSchedule' smart constructor.
data DeleteSchedule = DeleteSchedule'
  { -- | Id of the channel whose schedule is being deleted.
    channelId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelId', 'deleteSchedule_channelId' - Id of the channel whose schedule is being deleted.
newDeleteSchedule ::
  -- | 'channelId'
  Core.Text ->
  DeleteSchedule
newDeleteSchedule pChannelId_ =
  DeleteSchedule' {channelId = pChannelId_}

-- | Id of the channel whose schedule is being deleted.
deleteSchedule_channelId :: Lens.Lens' DeleteSchedule Core.Text
deleteSchedule_channelId = Lens.lens (\DeleteSchedule' {channelId} -> channelId) (\s@DeleteSchedule' {} a -> s {channelId = a} :: DeleteSchedule)

instance Core.AWSRequest DeleteSchedule where
  type
    AWSResponse DeleteSchedule =
      DeleteScheduleResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteScheduleResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteSchedule

instance Core.NFData DeleteSchedule

instance Core.ToHeaders DeleteSchedule where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteSchedule where
  toPath DeleteSchedule' {..} =
    Core.mconcat
      ["/prod/channels/", Core.toBS channelId, "/schedule"]

instance Core.ToQuery DeleteSchedule where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for DeleteScheduleResponse
--
-- /See:/ 'newDeleteScheduleResponse' smart constructor.
data DeleteScheduleResponse = DeleteScheduleResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteScheduleResponse_httpStatus' - The response's http status code.
newDeleteScheduleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteScheduleResponse
newDeleteScheduleResponse pHttpStatus_ =
  DeleteScheduleResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteScheduleResponse_httpStatus :: Lens.Lens' DeleteScheduleResponse Core.Int
deleteScheduleResponse_httpStatus = Lens.lens (\DeleteScheduleResponse' {httpStatus} -> httpStatus) (\s@DeleteScheduleResponse' {} a -> s {httpStatus = a} :: DeleteScheduleResponse)

instance Core.NFData DeleteScheduleResponse
