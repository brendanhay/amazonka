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
-- Module      : Amazonka.Lambda.Types.InvokeWithResponseStreamResponseEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.InvokeWithResponseStreamResponseEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types.InvokeResponseStreamUpdate
import Amazonka.Lambda.Types.InvokeWithResponseStreamCompleteEvent
import qualified Amazonka.Prelude as Prelude

-- | An object that includes a chunk of the response payload. When the stream
-- has ended, Lambda includes a @InvokeComplete@ object.
--
-- /See:/ 'newInvokeWithResponseStreamResponseEvent' smart constructor.
data InvokeWithResponseStreamResponseEvent = InvokeWithResponseStreamResponseEvent'
  { -- | An object that\'s returned when the stream has ended and all the payload
    -- chunks have been returned.
    invokeComplete :: Prelude.Maybe InvokeWithResponseStreamCompleteEvent,
    -- | A chunk of the streamed response payload.
    payloadChunk :: Prelude.Maybe InvokeResponseStreamUpdate
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InvokeWithResponseStreamResponseEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invokeComplete', 'invokeWithResponseStreamResponseEvent_invokeComplete' - An object that\'s returned when the stream has ended and all the payload
-- chunks have been returned.
--
-- 'payloadChunk', 'invokeWithResponseStreamResponseEvent_payloadChunk' - A chunk of the streamed response payload.
newInvokeWithResponseStreamResponseEvent ::
  InvokeWithResponseStreamResponseEvent
newInvokeWithResponseStreamResponseEvent =
  InvokeWithResponseStreamResponseEvent'
    { invokeComplete =
        Prelude.Nothing,
      payloadChunk = Prelude.Nothing
    }

-- | An object that\'s returned when the stream has ended and all the payload
-- chunks have been returned.
invokeWithResponseStreamResponseEvent_invokeComplete :: Lens.Lens' InvokeWithResponseStreamResponseEvent (Prelude.Maybe InvokeWithResponseStreamCompleteEvent)
invokeWithResponseStreamResponseEvent_invokeComplete = Lens.lens (\InvokeWithResponseStreamResponseEvent' {invokeComplete} -> invokeComplete) (\s@InvokeWithResponseStreamResponseEvent' {} a -> s {invokeComplete = a} :: InvokeWithResponseStreamResponseEvent)

-- | A chunk of the streamed response payload.
invokeWithResponseStreamResponseEvent_payloadChunk :: Lens.Lens' InvokeWithResponseStreamResponseEvent (Prelude.Maybe InvokeResponseStreamUpdate)
invokeWithResponseStreamResponseEvent_payloadChunk = Lens.lens (\InvokeWithResponseStreamResponseEvent' {payloadChunk} -> payloadChunk) (\s@InvokeWithResponseStreamResponseEvent' {} a -> s {payloadChunk = a} :: InvokeWithResponseStreamResponseEvent)

instance
  Data.FromJSON
    InvokeWithResponseStreamResponseEvent
  where
  parseJSON =
    Data.withObject
      "InvokeWithResponseStreamResponseEvent"
      ( \x ->
          InvokeWithResponseStreamResponseEvent'
            Prelude.<$> (x Data..:? "InvokeComplete")
            Prelude.<*> (x Data..:? "PayloadChunk")
      )

instance
  Prelude.Hashable
    InvokeWithResponseStreamResponseEvent
  where
  hashWithSalt
    _salt
    InvokeWithResponseStreamResponseEvent' {..} =
      _salt
        `Prelude.hashWithSalt` invokeComplete
        `Prelude.hashWithSalt` payloadChunk

instance
  Prelude.NFData
    InvokeWithResponseStreamResponseEvent
  where
  rnf InvokeWithResponseStreamResponseEvent' {..} =
    Prelude.rnf invokeComplete
      `Prelude.seq` Prelude.rnf payloadChunk
