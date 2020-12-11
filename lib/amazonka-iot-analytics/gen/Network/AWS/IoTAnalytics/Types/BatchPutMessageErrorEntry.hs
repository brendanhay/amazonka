-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.BatchPutMessageErrorEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.BatchPutMessageErrorEntry
  ( BatchPutMessageErrorEntry (..),

    -- * Smart constructor
    mkBatchPutMessageErrorEntry,

    -- * Lenses
    bpmeeErrorCode,
    bpmeeErrorMessage,
    bpmeeMessageId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains informations about errors.
--
-- /See:/ 'mkBatchPutMessageErrorEntry' smart constructor.
data BatchPutMessageErrorEntry = BatchPutMessageErrorEntry'
  { errorCode ::
      Lude.Maybe Lude.Text,
    errorMessage :: Lude.Maybe Lude.Text,
    messageId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchPutMessageErrorEntry' with the minimum fields required to make a request.
--
-- * 'errorCode' - The code associated with the error.
-- * 'errorMessage' - The message associated with the error.
-- * 'messageId' - The ID of the message that caused the error. See the value corresponding to the @messageId@ key in the message object.
mkBatchPutMessageErrorEntry ::
  BatchPutMessageErrorEntry
mkBatchPutMessageErrorEntry =
  BatchPutMessageErrorEntry'
    { errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing,
      messageId = Lude.Nothing
    }

-- | The code associated with the error.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpmeeErrorCode :: Lens.Lens' BatchPutMessageErrorEntry (Lude.Maybe Lude.Text)
bpmeeErrorCode = Lens.lens (errorCode :: BatchPutMessageErrorEntry -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: BatchPutMessageErrorEntry)
{-# DEPRECATED bpmeeErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The message associated with the error.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpmeeErrorMessage :: Lens.Lens' BatchPutMessageErrorEntry (Lude.Maybe Lude.Text)
bpmeeErrorMessage = Lens.lens (errorMessage :: BatchPutMessageErrorEntry -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: BatchPutMessageErrorEntry)
{-# DEPRECATED bpmeeErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The ID of the message that caused the error. See the value corresponding to the @messageId@ key in the message object.
--
-- /Note:/ Consider using 'messageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpmeeMessageId :: Lens.Lens' BatchPutMessageErrorEntry (Lude.Maybe Lude.Text)
bpmeeMessageId = Lens.lens (messageId :: BatchPutMessageErrorEntry -> Lude.Maybe Lude.Text) (\s a -> s {messageId = a} :: BatchPutMessageErrorEntry)
{-# DEPRECATED bpmeeMessageId "Use generic-lens or generic-optics with 'messageId' instead." #-}

instance Lude.FromJSON BatchPutMessageErrorEntry where
  parseJSON =
    Lude.withObject
      "BatchPutMessageErrorEntry"
      ( \x ->
          BatchPutMessageErrorEntry'
            Lude.<$> (x Lude..:? "errorCode")
            Lude.<*> (x Lude..:? "errorMessage")
            Lude.<*> (x Lude..:? "messageId")
      )
