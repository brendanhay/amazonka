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
-- Module      : Network.AWS.IoTAnalytics.Types.BatchPutMessageErrorEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.BatchPutMessageErrorEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains informations about errors.
--
-- /See:/ 'newBatchPutMessageErrorEntry' smart constructor.
data BatchPutMessageErrorEntry = BatchPutMessageErrorEntry'
  { -- | The ID of the message that caused the error. See the value corresponding
    -- to the @messageId@ key in the message object.
    messageId :: Core.Maybe Core.Text,
    -- | The message associated with the error.
    errorMessage :: Core.Maybe Core.Text,
    -- | The code associated with the error.
    errorCode :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchPutMessageErrorEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageId', 'batchPutMessageErrorEntry_messageId' - The ID of the message that caused the error. See the value corresponding
-- to the @messageId@ key in the message object.
--
-- 'errorMessage', 'batchPutMessageErrorEntry_errorMessage' - The message associated with the error.
--
-- 'errorCode', 'batchPutMessageErrorEntry_errorCode' - The code associated with the error.
newBatchPutMessageErrorEntry ::
  BatchPutMessageErrorEntry
newBatchPutMessageErrorEntry =
  BatchPutMessageErrorEntry'
    { messageId =
        Core.Nothing,
      errorMessage = Core.Nothing,
      errorCode = Core.Nothing
    }

-- | The ID of the message that caused the error. See the value corresponding
-- to the @messageId@ key in the message object.
batchPutMessageErrorEntry_messageId :: Lens.Lens' BatchPutMessageErrorEntry (Core.Maybe Core.Text)
batchPutMessageErrorEntry_messageId = Lens.lens (\BatchPutMessageErrorEntry' {messageId} -> messageId) (\s@BatchPutMessageErrorEntry' {} a -> s {messageId = a} :: BatchPutMessageErrorEntry)

-- | The message associated with the error.
batchPutMessageErrorEntry_errorMessage :: Lens.Lens' BatchPutMessageErrorEntry (Core.Maybe Core.Text)
batchPutMessageErrorEntry_errorMessage = Lens.lens (\BatchPutMessageErrorEntry' {errorMessage} -> errorMessage) (\s@BatchPutMessageErrorEntry' {} a -> s {errorMessage = a} :: BatchPutMessageErrorEntry)

-- | The code associated with the error.
batchPutMessageErrorEntry_errorCode :: Lens.Lens' BatchPutMessageErrorEntry (Core.Maybe Core.Text)
batchPutMessageErrorEntry_errorCode = Lens.lens (\BatchPutMessageErrorEntry' {errorCode} -> errorCode) (\s@BatchPutMessageErrorEntry' {} a -> s {errorCode = a} :: BatchPutMessageErrorEntry)

instance Core.FromJSON BatchPutMessageErrorEntry where
  parseJSON =
    Core.withObject
      "BatchPutMessageErrorEntry"
      ( \x ->
          BatchPutMessageErrorEntry'
            Core.<$> (x Core..:? "messageId")
            Core.<*> (x Core..:? "errorMessage")
            Core.<*> (x Core..:? "errorCode")
      )

instance Core.Hashable BatchPutMessageErrorEntry

instance Core.NFData BatchPutMessageErrorEntry
