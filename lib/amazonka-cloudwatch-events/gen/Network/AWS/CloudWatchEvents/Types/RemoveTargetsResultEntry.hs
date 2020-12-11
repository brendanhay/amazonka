-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.RemoveTargetsResultEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.RemoveTargetsResultEntry
  ( RemoveTargetsResultEntry (..),

    -- * Smart constructor
    mkRemoveTargetsResultEntry,

    -- * Lenses
    rtreTargetId,
    rtreErrorCode,
    rtreErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a target that failed to be removed from a rule.
--
-- /See:/ 'mkRemoveTargetsResultEntry' smart constructor.
data RemoveTargetsResultEntry = RemoveTargetsResultEntry'
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

-- | Creates a value of 'RemoveTargetsResultEntry' with the minimum fields required to make a request.
--
-- * 'errorCode' - The error code that indicates why the target removal failed. If the value is @ConcurrentModificationException@ , too many requests were made at the same time.
-- * 'errorMessage' - The error message that explains why the target removal failed.
-- * 'targetId' - The ID of the target.
mkRemoveTargetsResultEntry ::
  RemoveTargetsResultEntry
mkRemoveTargetsResultEntry =
  RemoveTargetsResultEntry'
    { targetId = Lude.Nothing,
      errorCode = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

-- | The ID of the target.
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtreTargetId :: Lens.Lens' RemoveTargetsResultEntry (Lude.Maybe Lude.Text)
rtreTargetId = Lens.lens (targetId :: RemoveTargetsResultEntry -> Lude.Maybe Lude.Text) (\s a -> s {targetId = a} :: RemoveTargetsResultEntry)
{-# DEPRECATED rtreTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

-- | The error code that indicates why the target removal failed. If the value is @ConcurrentModificationException@ , too many requests were made at the same time.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtreErrorCode :: Lens.Lens' RemoveTargetsResultEntry (Lude.Maybe Lude.Text)
rtreErrorCode = Lens.lens (errorCode :: RemoveTargetsResultEntry -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: RemoveTargetsResultEntry)
{-# DEPRECATED rtreErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message that explains why the target removal failed.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtreErrorMessage :: Lens.Lens' RemoveTargetsResultEntry (Lude.Maybe Lude.Text)
rtreErrorMessage = Lens.lens (errorMessage :: RemoveTargetsResultEntry -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: RemoveTargetsResultEntry)
{-# DEPRECATED rtreErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON RemoveTargetsResultEntry where
  parseJSON =
    Lude.withObject
      "RemoveTargetsResultEntry"
      ( \x ->
          RemoveTargetsResultEntry'
            Lude.<$> (x Lude..:? "TargetId")
            Lude.<*> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "ErrorMessage")
      )
