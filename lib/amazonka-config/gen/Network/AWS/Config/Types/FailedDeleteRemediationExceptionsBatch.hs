{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.FailedDeleteRemediationExceptionsBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.FailedDeleteRemediationExceptionsBatch
  ( FailedDeleteRemediationExceptionsBatch (..),

    -- * Smart constructor
    mkFailedDeleteRemediationExceptionsBatch,

    -- * Lenses
    fdrebFailureMessage,
    fdrebFailedItems,
  )
where

import Network.AWS.Config.Types.RemediationExceptionResourceKey
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | List of each of the failed delete remediation exceptions with specific reasons.
--
-- /See:/ 'mkFailedDeleteRemediationExceptionsBatch' smart constructor.
data FailedDeleteRemediationExceptionsBatch = FailedDeleteRemediationExceptionsBatch'
  { failureMessage ::
      Lude.Maybe
        Lude.Text,
    failedItems ::
      Lude.Maybe
        ( Lude.NonEmpty
            RemediationExceptionResourceKey
        )
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FailedDeleteRemediationExceptionsBatch' with the minimum fields required to make a request.
--
-- * 'failedItems' - Returns remediation exception resource key object of the failed items.
-- * 'failureMessage' - Returns a failure message for delete remediation exception. For example, AWS Config creates an exception due to an internal error.
mkFailedDeleteRemediationExceptionsBatch ::
  FailedDeleteRemediationExceptionsBatch
mkFailedDeleteRemediationExceptionsBatch =
  FailedDeleteRemediationExceptionsBatch'
    { failureMessage =
        Lude.Nothing,
      failedItems = Lude.Nothing
    }

-- | Returns a failure message for delete remediation exception. For example, AWS Config creates an exception due to an internal error.
--
-- /Note:/ Consider using 'failureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdrebFailureMessage :: Lens.Lens' FailedDeleteRemediationExceptionsBatch (Lude.Maybe Lude.Text)
fdrebFailureMessage = Lens.lens (failureMessage :: FailedDeleteRemediationExceptionsBatch -> Lude.Maybe Lude.Text) (\s a -> s {failureMessage = a} :: FailedDeleteRemediationExceptionsBatch)
{-# DEPRECATED fdrebFailureMessage "Use generic-lens or generic-optics with 'failureMessage' instead." #-}

-- | Returns remediation exception resource key object of the failed items.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdrebFailedItems :: Lens.Lens' FailedDeleteRemediationExceptionsBatch (Lude.Maybe (Lude.NonEmpty RemediationExceptionResourceKey))
fdrebFailedItems = Lens.lens (failedItems :: FailedDeleteRemediationExceptionsBatch -> Lude.Maybe (Lude.NonEmpty RemediationExceptionResourceKey)) (\s a -> s {failedItems = a} :: FailedDeleteRemediationExceptionsBatch)
{-# DEPRECATED fdrebFailedItems "Use generic-lens or generic-optics with 'failedItems' instead." #-}

instance Lude.FromJSON FailedDeleteRemediationExceptionsBatch where
  parseJSON =
    Lude.withObject
      "FailedDeleteRemediationExceptionsBatch"
      ( \x ->
          FailedDeleteRemediationExceptionsBatch'
            Lude.<$> (x Lude..:? "FailureMessage") Lude.<*> (x Lude..:? "FailedItems")
      )
