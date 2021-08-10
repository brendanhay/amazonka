{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SendUsersMessageResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SendUsersMessageResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EndpointMessageResult
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about which users and endpoints a message was sent
-- to.
--
-- /See:/ 'newSendUsersMessageResponse' smart constructor.
data SendUsersMessageResponse = SendUsersMessageResponse'
  { -- | An object that indicates which endpoints the message was sent to, for
    -- each user. The object lists user IDs and, for each user ID, provides the
    -- endpoint IDs that the message was sent to. For each endpoint ID, it
    -- provides an EndpointMessageResult object.
    result :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text EndpointMessageResult)),
    -- | The unique identifier that was assigned to the message request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the application that was used to send the
    -- message.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendUsersMessageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'result', 'sendUsersMessageResponse_result' - An object that indicates which endpoints the message was sent to, for
-- each user. The object lists user IDs and, for each user ID, provides the
-- endpoint IDs that the message was sent to. For each endpoint ID, it
-- provides an EndpointMessageResult object.
--
-- 'requestId', 'sendUsersMessageResponse_requestId' - The unique identifier that was assigned to the message request.
--
-- 'applicationId', 'sendUsersMessageResponse_applicationId' - The unique identifier for the application that was used to send the
-- message.
newSendUsersMessageResponse ::
  -- | 'applicationId'
  Prelude.Text ->
  SendUsersMessageResponse
newSendUsersMessageResponse pApplicationId_ =
  SendUsersMessageResponse'
    { result = Prelude.Nothing,
      requestId = Prelude.Nothing,
      applicationId = pApplicationId_
    }

-- | An object that indicates which endpoints the message was sent to, for
-- each user. The object lists user IDs and, for each user ID, provides the
-- endpoint IDs that the message was sent to. For each endpoint ID, it
-- provides an EndpointMessageResult object.
sendUsersMessageResponse_result :: Lens.Lens' SendUsersMessageResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text EndpointMessageResult)))
sendUsersMessageResponse_result = Lens.lens (\SendUsersMessageResponse' {result} -> result) (\s@SendUsersMessageResponse' {} a -> s {result = a} :: SendUsersMessageResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The unique identifier that was assigned to the message request.
sendUsersMessageResponse_requestId :: Lens.Lens' SendUsersMessageResponse (Prelude.Maybe Prelude.Text)
sendUsersMessageResponse_requestId = Lens.lens (\SendUsersMessageResponse' {requestId} -> requestId) (\s@SendUsersMessageResponse' {} a -> s {requestId = a} :: SendUsersMessageResponse)

-- | The unique identifier for the application that was used to send the
-- message.
sendUsersMessageResponse_applicationId :: Lens.Lens' SendUsersMessageResponse Prelude.Text
sendUsersMessageResponse_applicationId = Lens.lens (\SendUsersMessageResponse' {applicationId} -> applicationId) (\s@SendUsersMessageResponse' {} a -> s {applicationId = a} :: SendUsersMessageResponse)

instance Core.FromJSON SendUsersMessageResponse where
  parseJSON =
    Core.withObject
      "SendUsersMessageResponse"
      ( \x ->
          SendUsersMessageResponse'
            Prelude.<$> (x Core..:? "Result" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "RequestId")
            Prelude.<*> (x Core..: "ApplicationId")
      )

instance Prelude.Hashable SendUsersMessageResponse

instance Prelude.NFData SendUsersMessageResponse
