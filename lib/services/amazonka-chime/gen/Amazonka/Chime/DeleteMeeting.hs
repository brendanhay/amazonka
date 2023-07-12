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
-- Module      : Amazonka.Chime.DeleteMeeting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Amazon Chime SDK meeting. The operation deletes
-- all attendees, disconnects all clients, and prevents new clients from
-- joining the meeting. For more information about the Amazon Chime SDK,
-- see
-- <https://docs.aws.amazon.com/chime/latest/dg/meetings-sdk.html Using the Amazon Chime SDK>
-- in the /Amazon Chime Developer Guide/.
module Amazonka.Chime.DeleteMeeting
  ( -- * Creating a Request
    DeleteMeeting (..),
    newDeleteMeeting,

    -- * Request Lenses
    deleteMeeting_meetingId,

    -- * Destructuring the Response
    DeleteMeetingResponse (..),
    newDeleteMeetingResponse,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteMeeting' smart constructor.
data DeleteMeeting = DeleteMeeting'
  { -- | The Amazon Chime SDK meeting ID.
    meetingId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMeeting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meetingId', 'deleteMeeting_meetingId' - The Amazon Chime SDK meeting ID.
newDeleteMeeting ::
  -- | 'meetingId'
  Prelude.Text ->
  DeleteMeeting
newDeleteMeeting pMeetingId_ =
  DeleteMeeting' {meetingId = pMeetingId_}

-- | The Amazon Chime SDK meeting ID.
deleteMeeting_meetingId :: Lens.Lens' DeleteMeeting Prelude.Text
deleteMeeting_meetingId = Lens.lens (\DeleteMeeting' {meetingId} -> meetingId) (\s@DeleteMeeting' {} a -> s {meetingId = a} :: DeleteMeeting)

instance Core.AWSRequest DeleteMeeting where
  type
    AWSResponse DeleteMeeting =
      DeleteMeetingResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteMeetingResponse'

instance Prelude.Hashable DeleteMeeting where
  hashWithSalt _salt DeleteMeeting' {..} =
    _salt `Prelude.hashWithSalt` meetingId

instance Prelude.NFData DeleteMeeting where
  rnf DeleteMeeting' {..} = Prelude.rnf meetingId

instance Data.ToHeaders DeleteMeeting where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteMeeting where
  toPath DeleteMeeting' {..} =
    Prelude.mconcat ["/meetings/", Data.toBS meetingId]

instance Data.ToQuery DeleteMeeting where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMeetingResponse' smart constructor.
data DeleteMeetingResponse = DeleteMeetingResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMeetingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteMeetingResponse ::
  DeleteMeetingResponse
newDeleteMeetingResponse = DeleteMeetingResponse'

instance Prelude.NFData DeleteMeetingResponse where
  rnf _ = ()
