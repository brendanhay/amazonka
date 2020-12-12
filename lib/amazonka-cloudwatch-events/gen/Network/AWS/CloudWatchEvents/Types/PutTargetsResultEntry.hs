{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.PutTargetsResultEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.PutTargetsResultEntry
  ( PutTargetsResultEntry (..),

    -- * Smart constructor
    mkPutTargetsResultEntry,

    -- * Lenses
    ptreTargetId,
    ptreErrorCode,
    ptreErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a target that failed to be added to a rule.
--
-- /See:/ 'mkPutTargetsResultEntry' smart constructor.
data PutTargetsResultEntry = PutTargetsResultEntry'
  { targetId ::
      Lude.Maybe Lude.Text,
    errorCode :: Lude.Maybe Lude.Text,
    errorMessage :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutTargetsResultEntry' with the minimum fields required to make a request.
--
-- * 'errorCode' - The error code that indicates why the target addition failed. If the value is @ConcurrentModificationException@ , too many requests were made at the same time.
-- * 'errorMessage' - The error message that explains why the target addition failed.
-- * 'targetId' - The ID of the target.
mkPutTargetsResultEntry ::
  PutTargetsResultEntry
mkPutTargetsResultEntry =
  PutTargetsResultEntry'
    { targetId = Lude.Nothing,
      errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | The ID of the target.
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptreTargetId :: Lens.Lens' PutTargetsResultEntry (Lude.Maybe Lude.Text)
ptreTargetId = Lens.lens (targetId :: PutTargetsResultEntry -> Lude.Maybe Lude.Text) (\s a -> s {targetId = a} :: PutTargetsResultEntry)
{-# DEPRECATED ptreTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

-- | The error code that indicates why the target addition failed. If the value is @ConcurrentModificationException@ , too many requests were made at the same time.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptreErrorCode :: Lens.Lens' PutTargetsResultEntry (Lude.Maybe Lude.Text)
ptreErrorCode = Lens.lens (errorCode :: PutTargetsResultEntry -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: PutTargetsResultEntry)
{-# DEPRECATED ptreErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message that explains why the target addition failed.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptreErrorMessage :: Lens.Lens' PutTargetsResultEntry (Lude.Maybe Lude.Text)
ptreErrorMessage = Lens.lens (errorMessage :: PutTargetsResultEntry -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: PutTargetsResultEntry)
{-# DEPRECATED ptreErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON PutTargetsResultEntry where
  parseJSON =
    Lude.withObject
      "PutTargetsResultEntry"
      ( \x ->
          PutTargetsResultEntry'
            Lude.<$> (x Lude..:? "TargetId")
            Lude.<*> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "ErrorMessage")
      )
