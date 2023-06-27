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
-- Module      : Amazonka.Chime.CreateMeetingDialOut
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uses the join token and call metadata in a meeting request (From number,
-- To number, and so forth) to initiate an outbound call to a public
-- switched telephone network (PSTN) and join them into a Chime meeting.
-- Also ensures that the From number belongs to the customer.
--
-- To play welcome audio or implement an interactive voice response (IVR),
-- use the @CreateSipMediaApplicationCall@ action with the corresponding
-- SIP media application ID.
module Amazonka.Chime.CreateMeetingDialOut
  ( -- * Creating a Request
    CreateMeetingDialOut (..),
    newCreateMeetingDialOut,

    -- * Request Lenses
    createMeetingDialOut_meetingId,
    createMeetingDialOut_fromPhoneNumber,
    createMeetingDialOut_toPhoneNumber,
    createMeetingDialOut_joinToken,

    -- * Destructuring the Response
    CreateMeetingDialOutResponse (..),
    newCreateMeetingDialOutResponse,

    -- * Response Lenses
    createMeetingDialOutResponse_transactionId,
    createMeetingDialOutResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateMeetingDialOut' smart constructor.
data CreateMeetingDialOut = CreateMeetingDialOut'
  { -- | The Amazon Chime SDK meeting ID.
    meetingId :: Prelude.Text,
    -- | Phone number used as the caller ID when the remote party receives a
    -- call.
    fromPhoneNumber :: Data.Sensitive Prelude.Text,
    -- | Phone number called when inviting someone to a meeting.
    toPhoneNumber :: Data.Sensitive Prelude.Text,
    -- | Token used by the Amazon Chime SDK attendee. Call the
    -- <https://docs.aws.amazon.com/chime/latest/APIReference/API_CreateAttendee.html CreateAttendee>
    -- action to get a join token.
    joinToken :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMeetingDialOut' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meetingId', 'createMeetingDialOut_meetingId' - The Amazon Chime SDK meeting ID.
--
-- 'fromPhoneNumber', 'createMeetingDialOut_fromPhoneNumber' - Phone number used as the caller ID when the remote party receives a
-- call.
--
-- 'toPhoneNumber', 'createMeetingDialOut_toPhoneNumber' - Phone number called when inviting someone to a meeting.
--
-- 'joinToken', 'createMeetingDialOut_joinToken' - Token used by the Amazon Chime SDK attendee. Call the
-- <https://docs.aws.amazon.com/chime/latest/APIReference/API_CreateAttendee.html CreateAttendee>
-- action to get a join token.
newCreateMeetingDialOut ::
  -- | 'meetingId'
  Prelude.Text ->
  -- | 'fromPhoneNumber'
  Prelude.Text ->
  -- | 'toPhoneNumber'
  Prelude.Text ->
  -- | 'joinToken'
  Prelude.Text ->
  CreateMeetingDialOut
newCreateMeetingDialOut
  pMeetingId_
  pFromPhoneNumber_
  pToPhoneNumber_
  pJoinToken_ =
    CreateMeetingDialOut'
      { meetingId = pMeetingId_,
        fromPhoneNumber =
          Data._Sensitive Lens.# pFromPhoneNumber_,
        toPhoneNumber =
          Data._Sensitive Lens.# pToPhoneNumber_,
        joinToken = Data._Sensitive Lens.# pJoinToken_
      }

-- | The Amazon Chime SDK meeting ID.
createMeetingDialOut_meetingId :: Lens.Lens' CreateMeetingDialOut Prelude.Text
createMeetingDialOut_meetingId = Lens.lens (\CreateMeetingDialOut' {meetingId} -> meetingId) (\s@CreateMeetingDialOut' {} a -> s {meetingId = a} :: CreateMeetingDialOut)

-- | Phone number used as the caller ID when the remote party receives a
-- call.
createMeetingDialOut_fromPhoneNumber :: Lens.Lens' CreateMeetingDialOut Prelude.Text
createMeetingDialOut_fromPhoneNumber = Lens.lens (\CreateMeetingDialOut' {fromPhoneNumber} -> fromPhoneNumber) (\s@CreateMeetingDialOut' {} a -> s {fromPhoneNumber = a} :: CreateMeetingDialOut) Prelude.. Data._Sensitive

-- | Phone number called when inviting someone to a meeting.
createMeetingDialOut_toPhoneNumber :: Lens.Lens' CreateMeetingDialOut Prelude.Text
createMeetingDialOut_toPhoneNumber = Lens.lens (\CreateMeetingDialOut' {toPhoneNumber} -> toPhoneNumber) (\s@CreateMeetingDialOut' {} a -> s {toPhoneNumber = a} :: CreateMeetingDialOut) Prelude.. Data._Sensitive

-- | Token used by the Amazon Chime SDK attendee. Call the
-- <https://docs.aws.amazon.com/chime/latest/APIReference/API_CreateAttendee.html CreateAttendee>
-- action to get a join token.
createMeetingDialOut_joinToken :: Lens.Lens' CreateMeetingDialOut Prelude.Text
createMeetingDialOut_joinToken = Lens.lens (\CreateMeetingDialOut' {joinToken} -> joinToken) (\s@CreateMeetingDialOut' {} a -> s {joinToken = a} :: CreateMeetingDialOut) Prelude.. Data._Sensitive

instance Core.AWSRequest CreateMeetingDialOut where
  type
    AWSResponse CreateMeetingDialOut =
      CreateMeetingDialOutResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMeetingDialOutResponse'
            Prelude.<$> (x Data..?> "TransactionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateMeetingDialOut where
  hashWithSalt _salt CreateMeetingDialOut' {..} =
    _salt
      `Prelude.hashWithSalt` meetingId
      `Prelude.hashWithSalt` fromPhoneNumber
      `Prelude.hashWithSalt` toPhoneNumber
      `Prelude.hashWithSalt` joinToken

instance Prelude.NFData CreateMeetingDialOut where
  rnf CreateMeetingDialOut' {..} =
    Prelude.rnf meetingId
      `Prelude.seq` Prelude.rnf fromPhoneNumber
      `Prelude.seq` Prelude.rnf toPhoneNumber
      `Prelude.seq` Prelude.rnf joinToken

instance Data.ToHeaders CreateMeetingDialOut where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateMeetingDialOut where
  toJSON CreateMeetingDialOut' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("FromPhoneNumber" Data..= fromPhoneNumber),
            Prelude.Just ("ToPhoneNumber" Data..= toPhoneNumber),
            Prelude.Just ("JoinToken" Data..= joinToken)
          ]
      )

instance Data.ToPath CreateMeetingDialOut where
  toPath CreateMeetingDialOut' {..} =
    Prelude.mconcat
      ["/meetings/", Data.toBS meetingId, "/dial-outs"]

instance Data.ToQuery CreateMeetingDialOut where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMeetingDialOutResponse' smart constructor.
data CreateMeetingDialOutResponse = CreateMeetingDialOutResponse'
  { -- | Unique ID that tracks API calls.
    transactionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMeetingDialOutResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transactionId', 'createMeetingDialOutResponse_transactionId' - Unique ID that tracks API calls.
--
-- 'httpStatus', 'createMeetingDialOutResponse_httpStatus' - The response's http status code.
newCreateMeetingDialOutResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateMeetingDialOutResponse
newCreateMeetingDialOutResponse pHttpStatus_ =
  CreateMeetingDialOutResponse'
    { transactionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Unique ID that tracks API calls.
createMeetingDialOutResponse_transactionId :: Lens.Lens' CreateMeetingDialOutResponse (Prelude.Maybe Prelude.Text)
createMeetingDialOutResponse_transactionId = Lens.lens (\CreateMeetingDialOutResponse' {transactionId} -> transactionId) (\s@CreateMeetingDialOutResponse' {} a -> s {transactionId = a} :: CreateMeetingDialOutResponse)

-- | The response's http status code.
createMeetingDialOutResponse_httpStatus :: Lens.Lens' CreateMeetingDialOutResponse Prelude.Int
createMeetingDialOutResponse_httpStatus = Lens.lens (\CreateMeetingDialOutResponse' {httpStatus} -> httpStatus) (\s@CreateMeetingDialOutResponse' {} a -> s {httpStatus = a} :: CreateMeetingDialOutResponse)

instance Prelude.NFData CreateMeetingDialOutResponse where
  rnf CreateMeetingDialOutResponse' {..} =
    Prelude.rnf transactionId
      `Prelude.seq` Prelude.rnf httpStatus
