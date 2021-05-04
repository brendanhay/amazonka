{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains informations about errors.
--
-- /See:/ 'newBatchPutMessageErrorEntry' smart constructor.
data BatchPutMessageErrorEntry = BatchPutMessageErrorEntry'
  { -- | The ID of the message that caused the error. See the value corresponding
    -- to the @messageId@ key in the message object.
    messageId :: Prelude.Maybe Prelude.Text,
    -- | The message associated with the error.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The code associated with the error.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The ID of the message that caused the error. See the value corresponding
-- to the @messageId@ key in the message object.
batchPutMessageErrorEntry_messageId :: Lens.Lens' BatchPutMessageErrorEntry (Prelude.Maybe Prelude.Text)
batchPutMessageErrorEntry_messageId = Lens.lens (\BatchPutMessageErrorEntry' {messageId} -> messageId) (\s@BatchPutMessageErrorEntry' {} a -> s {messageId = a} :: BatchPutMessageErrorEntry)

-- | The message associated with the error.
batchPutMessageErrorEntry_errorMessage :: Lens.Lens' BatchPutMessageErrorEntry (Prelude.Maybe Prelude.Text)
batchPutMessageErrorEntry_errorMessage = Lens.lens (\BatchPutMessageErrorEntry' {errorMessage} -> errorMessage) (\s@BatchPutMessageErrorEntry' {} a -> s {errorMessage = a} :: BatchPutMessageErrorEntry)

-- | The code associated with the error.
batchPutMessageErrorEntry_errorCode :: Lens.Lens' BatchPutMessageErrorEntry (Prelude.Maybe Prelude.Text)
batchPutMessageErrorEntry_errorCode = Lens.lens (\BatchPutMessageErrorEntry' {errorCode} -> errorCode) (\s@BatchPutMessageErrorEntry' {} a -> s {errorCode = a} :: BatchPutMessageErrorEntry)

instance Prelude.FromJSON BatchPutMessageErrorEntry where
  parseJSON =
    Prelude.withObject
      "BatchPutMessageErrorEntry"
      ( \x ->
          BatchPutMessageErrorEntry'
            Prelude.<$> (x Prelude..:? "messageId")
            Prelude.<*> (x Prelude..:? "errorMessage")
            Prelude.<*> (x Prelude..:? "errorCode")
      )

instance Prelude.Hashable BatchPutMessageErrorEntry

instance Prelude.NFData BatchPutMessageErrorEntry
