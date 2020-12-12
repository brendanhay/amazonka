{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.FailedRemediationBatch
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.FailedRemediationBatch
  ( FailedRemediationBatch (..),

    -- * Smart constructor
    mkFailedRemediationBatch,

    -- * Lenses
    frbFailureMessage,
    frbFailedItems,
  )
where

import Network.AWS.Config.Types.RemediationConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | List of each of the failed remediations with specific reasons.
--
-- /See:/ 'mkFailedRemediationBatch' smart constructor.
data FailedRemediationBatch = FailedRemediationBatch'
  { failureMessage ::
      Lude.Maybe Lude.Text,
    failedItems ::
      Lude.Maybe [RemediationConfiguration]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FailedRemediationBatch' with the minimum fields required to make a request.
--
-- * 'failedItems' - Returns remediation configurations of the failed items.
-- * 'failureMessage' - Returns a failure message. For example, the resource is already compliant.
mkFailedRemediationBatch ::
  FailedRemediationBatch
mkFailedRemediationBatch =
  FailedRemediationBatch'
    { failureMessage = Lude.Nothing,
      failedItems = Lude.Nothing
    }

-- | Returns a failure message. For example, the resource is already compliant.
--
-- /Note:/ Consider using 'failureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frbFailureMessage :: Lens.Lens' FailedRemediationBatch (Lude.Maybe Lude.Text)
frbFailureMessage = Lens.lens (failureMessage :: FailedRemediationBatch -> Lude.Maybe Lude.Text) (\s a -> s {failureMessage = a} :: FailedRemediationBatch)
{-# DEPRECATED frbFailureMessage "Use generic-lens or generic-optics with 'failureMessage' instead." #-}

-- | Returns remediation configurations of the failed items.
--
-- /Note:/ Consider using 'failedItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frbFailedItems :: Lens.Lens' FailedRemediationBatch (Lude.Maybe [RemediationConfiguration])
frbFailedItems = Lens.lens (failedItems :: FailedRemediationBatch -> Lude.Maybe [RemediationConfiguration]) (\s a -> s {failedItems = a} :: FailedRemediationBatch)
{-# DEPRECATED frbFailedItems "Use generic-lens or generic-optics with 'failedItems' instead." #-}

instance Lude.FromJSON FailedRemediationBatch where
  parseJSON =
    Lude.withObject
      "FailedRemediationBatch"
      ( \x ->
          FailedRemediationBatch'
            Lude.<$> (x Lude..:? "FailureMessage")
            Lude.<*> (x Lude..:? "FailedItems" Lude..!= Lude.mempty)
      )
