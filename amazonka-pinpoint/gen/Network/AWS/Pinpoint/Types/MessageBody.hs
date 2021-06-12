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
-- Module      : Network.AWS.Pinpoint.Types.MessageBody
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.MessageBody where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information about an API request or response.
--
-- /See:/ 'newMessageBody' smart constructor.
data MessageBody = MessageBody'
  { -- | The message that\'s returned from the API.
    message :: Core.Maybe Core.Text,
    -- | The unique identifier for the request or response.
    requestID :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MessageBody' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'messageBody_message' - The message that\'s returned from the API.
--
-- 'requestID', 'messageBody_requestID' - The unique identifier for the request or response.
newMessageBody ::
  MessageBody
newMessageBody =
  MessageBody'
    { message = Core.Nothing,
      requestID = Core.Nothing
    }

-- | The message that\'s returned from the API.
messageBody_message :: Lens.Lens' MessageBody (Core.Maybe Core.Text)
messageBody_message = Lens.lens (\MessageBody' {message} -> message) (\s@MessageBody' {} a -> s {message = a} :: MessageBody)

-- | The unique identifier for the request or response.
messageBody_requestID :: Lens.Lens' MessageBody (Core.Maybe Core.Text)
messageBody_requestID = Lens.lens (\MessageBody' {requestID} -> requestID) (\s@MessageBody' {} a -> s {requestID = a} :: MessageBody)

instance Core.FromJSON MessageBody where
  parseJSON =
    Core.withObject
      "MessageBody"
      ( \x ->
          MessageBody'
            Core.<$> (x Core..:? "Message")
            Core.<*> (x Core..:? "RequestID")
      )

instance Core.Hashable MessageBody

instance Core.NFData MessageBody
