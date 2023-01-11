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
-- Module      : Amazonka.Pinpoint.Types.SendUsersMessageResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.SendUsersMessageResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.EndpointMessageResult
import qualified Amazonka.Prelude as Prelude

-- | Provides information about which users and endpoints a message was sent
-- to.
--
-- /See:/ 'newSendUsersMessageResponse' smart constructor.
data SendUsersMessageResponse = SendUsersMessageResponse'
  { -- | The unique identifier that was assigned to the message request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | An object that indicates which endpoints the message was sent to, for
    -- each user. The object lists user IDs and, for each user ID, provides the
    -- endpoint IDs that the message was sent to. For each endpoint ID, it
    -- provides an EndpointMessageResult object.
    result :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text EndpointMessageResult)),
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
-- 'requestId', 'sendUsersMessageResponse_requestId' - The unique identifier that was assigned to the message request.
--
-- 'result', 'sendUsersMessageResponse_result' - An object that indicates which endpoints the message was sent to, for
-- each user. The object lists user IDs and, for each user ID, provides the
-- endpoint IDs that the message was sent to. For each endpoint ID, it
-- provides an EndpointMessageResult object.
--
-- 'applicationId', 'sendUsersMessageResponse_applicationId' - The unique identifier for the application that was used to send the
-- message.
newSendUsersMessageResponse ::
  -- | 'applicationId'
  Prelude.Text ->
  SendUsersMessageResponse
newSendUsersMessageResponse pApplicationId_ =
  SendUsersMessageResponse'
    { requestId =
        Prelude.Nothing,
      result = Prelude.Nothing,
      applicationId = pApplicationId_
    }

-- | The unique identifier that was assigned to the message request.
sendUsersMessageResponse_requestId :: Lens.Lens' SendUsersMessageResponse (Prelude.Maybe Prelude.Text)
sendUsersMessageResponse_requestId = Lens.lens (\SendUsersMessageResponse' {requestId} -> requestId) (\s@SendUsersMessageResponse' {} a -> s {requestId = a} :: SendUsersMessageResponse)

-- | An object that indicates which endpoints the message was sent to, for
-- each user. The object lists user IDs and, for each user ID, provides the
-- endpoint IDs that the message was sent to. For each endpoint ID, it
-- provides an EndpointMessageResult object.
sendUsersMessageResponse_result :: Lens.Lens' SendUsersMessageResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text EndpointMessageResult)))
sendUsersMessageResponse_result = Lens.lens (\SendUsersMessageResponse' {result} -> result) (\s@SendUsersMessageResponse' {} a -> s {result = a} :: SendUsersMessageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier for the application that was used to send the
-- message.
sendUsersMessageResponse_applicationId :: Lens.Lens' SendUsersMessageResponse Prelude.Text
sendUsersMessageResponse_applicationId = Lens.lens (\SendUsersMessageResponse' {applicationId} -> applicationId) (\s@SendUsersMessageResponse' {} a -> s {applicationId = a} :: SendUsersMessageResponse)

instance Data.FromJSON SendUsersMessageResponse where
  parseJSON =
    Data.withObject
      "SendUsersMessageResponse"
      ( \x ->
          SendUsersMessageResponse'
            Prelude.<$> (x Data..:? "RequestId")
            Prelude.<*> (x Data..:? "Result" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "ApplicationId")
      )

instance Prelude.Hashable SendUsersMessageResponse where
  hashWithSalt _salt SendUsersMessageResponse' {..} =
    _salt `Prelude.hashWithSalt` requestId
      `Prelude.hashWithSalt` result
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData SendUsersMessageResponse where
  rnf SendUsersMessageResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf result
      `Prelude.seq` Prelude.rnf applicationId
