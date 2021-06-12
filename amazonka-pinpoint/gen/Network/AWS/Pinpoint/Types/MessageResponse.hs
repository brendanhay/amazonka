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
-- Module      : Network.AWS.Pinpoint.Types.MessageResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.MessageResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EndpointMessageResult
import Network.AWS.Pinpoint.Types.MessageResult

-- | Provides information about the results of a request to send a message to
-- an endpoint address.
--
-- /See:/ 'newMessageResponse' smart constructor.
data MessageResponse = MessageResponse'
  { -- | A map that contains a multipart response for each address (email
    -- address, phone number, or push notification token) that the message was
    -- sent to. In the map, the address is the key and the result is the value.
    result :: Core.Maybe (Core.HashMap Core.Text MessageResult),
    -- | The identifier for the original request that the message was delivered
    -- for.
    requestId :: Core.Maybe Core.Text,
    -- | A map that contains a multipart response for each address that the
    -- message was sent to. In the map, the endpoint ID is the key and the
    -- result is the value.
    endpointResult :: Core.Maybe (Core.HashMap Core.Text EndpointMessageResult),
    -- | The unique identifier for the application that was used to send the
    -- message.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MessageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'result', 'messageResponse_result' - A map that contains a multipart response for each address (email
-- address, phone number, or push notification token) that the message was
-- sent to. In the map, the address is the key and the result is the value.
--
-- 'requestId', 'messageResponse_requestId' - The identifier for the original request that the message was delivered
-- for.
--
-- 'endpointResult', 'messageResponse_endpointResult' - A map that contains a multipart response for each address that the
-- message was sent to. In the map, the endpoint ID is the key and the
-- result is the value.
--
-- 'applicationId', 'messageResponse_applicationId' - The unique identifier for the application that was used to send the
-- message.
newMessageResponse ::
  -- | 'applicationId'
  Core.Text ->
  MessageResponse
newMessageResponse pApplicationId_ =
  MessageResponse'
    { result = Core.Nothing,
      requestId = Core.Nothing,
      endpointResult = Core.Nothing,
      applicationId = pApplicationId_
    }

-- | A map that contains a multipart response for each address (email
-- address, phone number, or push notification token) that the message was
-- sent to. In the map, the address is the key and the result is the value.
messageResponse_result :: Lens.Lens' MessageResponse (Core.Maybe (Core.HashMap Core.Text MessageResult))
messageResponse_result = Lens.lens (\MessageResponse' {result} -> result) (\s@MessageResponse' {} a -> s {result = a} :: MessageResponse) Core.. Lens.mapping Lens._Coerce

-- | The identifier for the original request that the message was delivered
-- for.
messageResponse_requestId :: Lens.Lens' MessageResponse (Core.Maybe Core.Text)
messageResponse_requestId = Lens.lens (\MessageResponse' {requestId} -> requestId) (\s@MessageResponse' {} a -> s {requestId = a} :: MessageResponse)

-- | A map that contains a multipart response for each address that the
-- message was sent to. In the map, the endpoint ID is the key and the
-- result is the value.
messageResponse_endpointResult :: Lens.Lens' MessageResponse (Core.Maybe (Core.HashMap Core.Text EndpointMessageResult))
messageResponse_endpointResult = Lens.lens (\MessageResponse' {endpointResult} -> endpointResult) (\s@MessageResponse' {} a -> s {endpointResult = a} :: MessageResponse) Core.. Lens.mapping Lens._Coerce

-- | The unique identifier for the application that was used to send the
-- message.
messageResponse_applicationId :: Lens.Lens' MessageResponse Core.Text
messageResponse_applicationId = Lens.lens (\MessageResponse' {applicationId} -> applicationId) (\s@MessageResponse' {} a -> s {applicationId = a} :: MessageResponse)

instance Core.FromJSON MessageResponse where
  parseJSON =
    Core.withObject
      "MessageResponse"
      ( \x ->
          MessageResponse'
            Core.<$> (x Core..:? "Result" Core..!= Core.mempty)
            Core.<*> (x Core..:? "RequestId")
            Core.<*> (x Core..:? "EndpointResult" Core..!= Core.mempty)
            Core.<*> (x Core..: "ApplicationId")
      )

instance Core.Hashable MessageResponse

instance Core.NFData MessageResponse
