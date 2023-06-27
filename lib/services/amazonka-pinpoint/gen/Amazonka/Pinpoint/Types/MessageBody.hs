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
-- Module      : Amazonka.Pinpoint.Types.MessageBody
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.MessageBody where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an API request or response.
--
-- /See:/ 'newMessageBody' smart constructor.
data MessageBody = MessageBody'
  { -- | The message that\'s returned from the API.
    message :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the request or response.
    requestID :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { message = Prelude.Nothing,
      requestID = Prelude.Nothing
    }

-- | The message that\'s returned from the API.
messageBody_message :: Lens.Lens' MessageBody (Prelude.Maybe Prelude.Text)
messageBody_message = Lens.lens (\MessageBody' {message} -> message) (\s@MessageBody' {} a -> s {message = a} :: MessageBody)

-- | The unique identifier for the request or response.
messageBody_requestID :: Lens.Lens' MessageBody (Prelude.Maybe Prelude.Text)
messageBody_requestID = Lens.lens (\MessageBody' {requestID} -> requestID) (\s@MessageBody' {} a -> s {requestID = a} :: MessageBody)

instance Data.FromJSON MessageBody where
  parseJSON =
    Data.withObject
      "MessageBody"
      ( \x ->
          MessageBody'
            Prelude.<$> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "RequestID")
      )

instance Prelude.Hashable MessageBody where
  hashWithSalt _salt MessageBody' {..} =
    _salt
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` requestID

instance Prelude.NFData MessageBody where
  rnf MessageBody' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf requestID
