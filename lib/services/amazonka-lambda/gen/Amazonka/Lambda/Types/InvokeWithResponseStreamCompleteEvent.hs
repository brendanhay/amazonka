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
-- Module      : Amazonka.Lambda.Types.InvokeWithResponseStreamCompleteEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.InvokeWithResponseStreamCompleteEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A response confirming that the event stream is complete.
--
-- /See:/ 'newInvokeWithResponseStreamCompleteEvent' smart constructor.
data InvokeWithResponseStreamCompleteEvent = InvokeWithResponseStreamCompleteEvent'
  { -- | An error code.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The details of any returned error.
    errorDetails :: Prelude.Maybe Prelude.Text,
    -- | The last 4 KB of the execution log, which is base64-encoded.
    logResult :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InvokeWithResponseStreamCompleteEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'invokeWithResponseStreamCompleteEvent_errorCode' - An error code.
--
-- 'errorDetails', 'invokeWithResponseStreamCompleteEvent_errorDetails' - The details of any returned error.
--
-- 'logResult', 'invokeWithResponseStreamCompleteEvent_logResult' - The last 4 KB of the execution log, which is base64-encoded.
newInvokeWithResponseStreamCompleteEvent ::
  InvokeWithResponseStreamCompleteEvent
newInvokeWithResponseStreamCompleteEvent =
  InvokeWithResponseStreamCompleteEvent'
    { errorCode =
        Prelude.Nothing,
      errorDetails = Prelude.Nothing,
      logResult = Prelude.Nothing
    }

-- | An error code.
invokeWithResponseStreamCompleteEvent_errorCode :: Lens.Lens' InvokeWithResponseStreamCompleteEvent (Prelude.Maybe Prelude.Text)
invokeWithResponseStreamCompleteEvent_errorCode = Lens.lens (\InvokeWithResponseStreamCompleteEvent' {errorCode} -> errorCode) (\s@InvokeWithResponseStreamCompleteEvent' {} a -> s {errorCode = a} :: InvokeWithResponseStreamCompleteEvent)

-- | The details of any returned error.
invokeWithResponseStreamCompleteEvent_errorDetails :: Lens.Lens' InvokeWithResponseStreamCompleteEvent (Prelude.Maybe Prelude.Text)
invokeWithResponseStreamCompleteEvent_errorDetails = Lens.lens (\InvokeWithResponseStreamCompleteEvent' {errorDetails} -> errorDetails) (\s@InvokeWithResponseStreamCompleteEvent' {} a -> s {errorDetails = a} :: InvokeWithResponseStreamCompleteEvent)

-- | The last 4 KB of the execution log, which is base64-encoded.
invokeWithResponseStreamCompleteEvent_logResult :: Lens.Lens' InvokeWithResponseStreamCompleteEvent (Prelude.Maybe Prelude.Text)
invokeWithResponseStreamCompleteEvent_logResult = Lens.lens (\InvokeWithResponseStreamCompleteEvent' {logResult} -> logResult) (\s@InvokeWithResponseStreamCompleteEvent' {} a -> s {logResult = a} :: InvokeWithResponseStreamCompleteEvent)

instance
  Data.FromJSON
    InvokeWithResponseStreamCompleteEvent
  where
  parseJSON =
    Data.withObject
      "InvokeWithResponseStreamCompleteEvent"
      ( \x ->
          InvokeWithResponseStreamCompleteEvent'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "ErrorDetails")
            Prelude.<*> (x Data..:? "LogResult")
      )

instance
  Prelude.Hashable
    InvokeWithResponseStreamCompleteEvent
  where
  hashWithSalt
    _salt
    InvokeWithResponseStreamCompleteEvent' {..} =
      _salt
        `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorDetails
        `Prelude.hashWithSalt` logResult

instance
  Prelude.NFData
    InvokeWithResponseStreamCompleteEvent
  where
  rnf InvokeWithResponseStreamCompleteEvent' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorDetails
      `Prelude.seq` Prelude.rnf logResult
