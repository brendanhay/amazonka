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
-- Module      : Amazonka.MediaLive.DeleteSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete all schedule actions on a channel.
module Amazonka.MediaLive.DeleteSchedule
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Placeholder documentation for DeleteScheduleRequest
--
-- /See:/ 'newDeleteSchedule' smart constructor.
data DeleteSchedule = DeleteSchedule'
  { -- | Id of the channel whose schedule is being deleted.
    channelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteSchedule
newDeleteSchedule pChannelId_ =
  DeleteSchedule' {channelId = pChannelId_}

-- | Id of the channel whose schedule is being deleted.
deleteSchedule_channelId :: Lens.Lens' DeleteSchedule Prelude.Text
deleteSchedule_channelId = Lens.lens (\DeleteSchedule' {channelId} -> channelId) (\s@DeleteSchedule' {} a -> s {channelId = a} :: DeleteSchedule)

instance Core.AWSRequest DeleteSchedule where
  type
    AWSResponse DeleteSchedule =
      DeleteScheduleResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteScheduleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSchedule where
  hashWithSalt _salt DeleteSchedule' {..} =
    _salt `Prelude.hashWithSalt` channelId

instance Prelude.NFData DeleteSchedule where
  rnf DeleteSchedule' {..} = Prelude.rnf channelId

instance Data.ToHeaders DeleteSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteSchedule where
  toPath DeleteSchedule' {..} =
    Prelude.mconcat
      ["/prod/channels/", Data.toBS channelId, "/schedule"]

instance Data.ToQuery DeleteSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for DeleteScheduleResponse
--
-- /See:/ 'newDeleteScheduleResponse' smart constructor.
data DeleteScheduleResponse = DeleteScheduleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteScheduleResponse
newDeleteScheduleResponse pHttpStatus_ =
  DeleteScheduleResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteScheduleResponse_httpStatus :: Lens.Lens' DeleteScheduleResponse Prelude.Int
deleteScheduleResponse_httpStatus = Lens.lens (\DeleteScheduleResponse' {httpStatus} -> httpStatus) (\s@DeleteScheduleResponse' {} a -> s {httpStatus = a} :: DeleteScheduleResponse)

instance Prelude.NFData DeleteScheduleResponse where
  rnf DeleteScheduleResponse' {..} =
    Prelude.rnf httpStatus
