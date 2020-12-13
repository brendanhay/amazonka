{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.PutRecordBatchResponseEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.PutRecordBatchResponseEntry
  ( PutRecordBatchResponseEntry (..),

    -- * Smart constructor
    mkPutRecordBatchResponseEntry,

    -- * Lenses
    prbreRecordId,
    prbreErrorCode,
    prbreErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the result for an individual record from a 'PutRecordBatch' request. If the record is successfully added to your delivery stream, it receives a record ID. If the record fails to be added to your delivery stream, the result includes an error code and an error message.
--
-- /See:/ 'mkPutRecordBatchResponseEntry' smart constructor.
data PutRecordBatchResponseEntry = PutRecordBatchResponseEntry'
  { -- | The ID of the record.
    recordId :: Lude.Maybe Lude.Text,
    -- | The error code for an individual record result.
    errorCode :: Lude.Maybe Lude.Text,
    -- | The error message for an individual record result.
    errorMessage :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRecordBatchResponseEntry' with the minimum fields required to make a request.
--
-- * 'recordId' - The ID of the record.
-- * 'errorCode' - The error code for an individual record result.
-- * 'errorMessage' - The error message for an individual record result.
mkPutRecordBatchResponseEntry ::
  PutRecordBatchResponseEntry
mkPutRecordBatchResponseEntry =
  PutRecordBatchResponseEntry'
    { recordId = Lude.Nothing,
      errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | The ID of the record.
--
-- /Note:/ Consider using 'recordId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prbreRecordId :: Lens.Lens' PutRecordBatchResponseEntry (Lude.Maybe Lude.Text)
prbreRecordId = Lens.lens (recordId :: PutRecordBatchResponseEntry -> Lude.Maybe Lude.Text) (\s a -> s {recordId = a} :: PutRecordBatchResponseEntry)
{-# DEPRECATED prbreRecordId "Use generic-lens or generic-optics with 'recordId' instead." #-}

-- | The error code for an individual record result.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prbreErrorCode :: Lens.Lens' PutRecordBatchResponseEntry (Lude.Maybe Lude.Text)
prbreErrorCode = Lens.lens (errorCode :: PutRecordBatchResponseEntry -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: PutRecordBatchResponseEntry)
{-# DEPRECATED prbreErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message for an individual record result.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prbreErrorMessage :: Lens.Lens' PutRecordBatchResponseEntry (Lude.Maybe Lude.Text)
prbreErrorMessage = Lens.lens (errorMessage :: PutRecordBatchResponseEntry -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: PutRecordBatchResponseEntry)
{-# DEPRECATED prbreErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON PutRecordBatchResponseEntry where
  parseJSON =
    Lude.withObject
      "PutRecordBatchResponseEntry"
      ( \x ->
          PutRecordBatchResponseEntry'
            Lude.<$> (x Lude..:? "RecordId")
            Lude.<*> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "ErrorMessage")
      )
