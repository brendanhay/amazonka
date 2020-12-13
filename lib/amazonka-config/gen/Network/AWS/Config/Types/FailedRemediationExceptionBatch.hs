{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.FailedRemediationExceptionBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.FailedRemediationExceptionBatch
  ( FailedRemediationExceptionBatch (..),

    -- * Smart constructor
    mkFailedRemediationExceptionBatch,

    -- * Lenses
    frebFailureMessage,
    frebFailedItems,
  )
where

import Network.AWS.Config.Types.RemediationException
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | List of each of the failed remediation exceptions with specific reasons.
--
-- /See:/ 'mkFailedRemediationExceptionBatch' smart constructor.
data FailedRemediationExceptionBatch = FailedRemediationExceptionBatch'
  { -- | Returns a failure message. For example, the auto-remediation has failed.
    failureMessage :: Lude.Maybe Lude.Text,
    -- | Returns remediation exception resource key object of the failed items.
    failedItems :: Lude.Maybe [RemediationException]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FailedRemediationExceptionBatch' with the minimum fields required to make a request.
--
-- * 'failureMessage' - Returns a failure message. For example, the auto-remediation has failed.
-- * 'failedItems' - Returns remediation exception resource key object of the failed items.
mkFailedRemediationExceptionBatch ::
  FailedRemediationExceptionBatch
mkFailedRemediationExceptionBatch =
  FailedRemediationExceptionBatch'
    { failureMessage = Lude.Nothing,
      failedItems = Lude.Nothing
    }

-- | Returns a failure message. For example, the auto-remediation has failed.
--
-- /Note:/ Consider using 'failureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frebFailureMessage :: Lens.Lens' FailedRemediationExceptionBatch (Lude.Maybe Lude.Text)
frebFailureMessage = Lens.lens (failureMessage :: FailedRemediationExceptionBatch -> Lude.Maybe Lude.Text) (\s a -> s {failureMessage = a} :: FailedRemediationExceptionBatch)
{-# DEPRECATED frebFailureMessage "Use generic-lens or generic-optics with 'failureMessage' instead." #-}

-- | Returns remediation exception resource key object of the failed items.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frebFailedItems :: Lens.Lens' FailedRemediationExceptionBatch (Lude.Maybe [RemediationException])
frebFailedItems = Lens.lens (failedItems :: FailedRemediationExceptionBatch -> Lude.Maybe [RemediationException]) (\s a -> s {failedItems = a} :: FailedRemediationExceptionBatch)
{-# DEPRECATED frebFailedItems "Use generic-lens or generic-optics with 'failedItems' instead." #-}

instance Lude.FromJSON FailedRemediationExceptionBatch where
  parseJSON =
    Lude.withObject
      "FailedRemediationExceptionBatch"
      ( \x ->
          FailedRemediationExceptionBatch'
            Lude.<$> (x Lude..:? "FailureMessage")
            Lude.<*> (x Lude..:? "FailedItems" Lude..!= Lude.mempty)
      )
