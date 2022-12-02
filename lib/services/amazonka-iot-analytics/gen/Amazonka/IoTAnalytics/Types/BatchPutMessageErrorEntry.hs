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
-- Module      : Amazonka.IoTAnalytics.Types.BatchPutMessageErrorEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.BatchPutMessageErrorEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains informations about errors.
--
-- /See:/ 'newBatchPutMessageErrorEntry' smart constructor.
data BatchPutMessageErrorEntry = BatchPutMessageErrorEntry'
  { -- | The message associated with the error.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the message that caused the error. See the value corresponding
    -- to the @messageId@ key in the message object.
    messageId :: Prelude.Maybe Prelude.Text,
    -- | The code associated with the error.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPutMessageErrorEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'batchPutMessageErrorEntry_errorMessage' - The message associated with the error.
--
-- 'messageId', 'batchPutMessageErrorEntry_messageId' - The ID of the message that caused the error. See the value corresponding
-- to the @messageId@ key in the message object.
--
-- 'errorCode', 'batchPutMessageErrorEntry_errorCode' - The code associated with the error.
newBatchPutMessageErrorEntry ::
  BatchPutMessageErrorEntry
newBatchPutMessageErrorEntry =
  BatchPutMessageErrorEntry'
    { errorMessage =
        Prelude.Nothing,
      messageId = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The message associated with the error.
batchPutMessageErrorEntry_errorMessage :: Lens.Lens' BatchPutMessageErrorEntry (Prelude.Maybe Prelude.Text)
batchPutMessageErrorEntry_errorMessage = Lens.lens (\BatchPutMessageErrorEntry' {errorMessage} -> errorMessage) (\s@BatchPutMessageErrorEntry' {} a -> s {errorMessage = a} :: BatchPutMessageErrorEntry)

-- | The ID of the message that caused the error. See the value corresponding
-- to the @messageId@ key in the message object.
batchPutMessageErrorEntry_messageId :: Lens.Lens' BatchPutMessageErrorEntry (Prelude.Maybe Prelude.Text)
batchPutMessageErrorEntry_messageId = Lens.lens (\BatchPutMessageErrorEntry' {messageId} -> messageId) (\s@BatchPutMessageErrorEntry' {} a -> s {messageId = a} :: BatchPutMessageErrorEntry)

-- | The code associated with the error.
batchPutMessageErrorEntry_errorCode :: Lens.Lens' BatchPutMessageErrorEntry (Prelude.Maybe Prelude.Text)
batchPutMessageErrorEntry_errorCode = Lens.lens (\BatchPutMessageErrorEntry' {errorCode} -> errorCode) (\s@BatchPutMessageErrorEntry' {} a -> s {errorCode = a} :: BatchPutMessageErrorEntry)

instance Data.FromJSON BatchPutMessageErrorEntry where
  parseJSON =
    Data.withObject
      "BatchPutMessageErrorEntry"
      ( \x ->
          BatchPutMessageErrorEntry'
            Prelude.<$> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..:? "messageId")
            Prelude.<*> (x Data..:? "errorCode")
      )

instance Prelude.Hashable BatchPutMessageErrorEntry where
  hashWithSalt _salt BatchPutMessageErrorEntry' {..} =
    _salt `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` messageId
      `Prelude.hashWithSalt` errorCode

instance Prelude.NFData BatchPutMessageErrorEntry where
  rnf BatchPutMessageErrorEntry' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf errorCode
