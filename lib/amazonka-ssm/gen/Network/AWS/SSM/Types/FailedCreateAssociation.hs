-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.FailedCreateAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.FailedCreateAssociation
  ( FailedCreateAssociation (..),

    -- * Smart constructor
    mkFailedCreateAssociation,

    -- * Lenses
    fcaEntry,
    fcaFault,
    fcaMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.CreateAssociationBatchRequestEntry
import Network.AWS.SSM.Types.Fault

-- | Describes a failed association.
--
-- /See:/ 'mkFailedCreateAssociation' smart constructor.
data FailedCreateAssociation = FailedCreateAssociation'
  { entry ::
      Lude.Maybe
        CreateAssociationBatchRequestEntry,
    fault :: Lude.Maybe Fault,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FailedCreateAssociation' with the minimum fields required to make a request.
--
-- * 'entry' - The association.
-- * 'fault' - The source of the failure.
-- * 'message' - A description of the failure.
mkFailedCreateAssociation ::
  FailedCreateAssociation
mkFailedCreateAssociation =
  FailedCreateAssociation'
    { entry = Lude.Nothing,
      fault = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The association.
--
-- /Note:/ Consider using 'entry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcaEntry :: Lens.Lens' FailedCreateAssociation (Lude.Maybe CreateAssociationBatchRequestEntry)
fcaEntry = Lens.lens (entry :: FailedCreateAssociation -> Lude.Maybe CreateAssociationBatchRequestEntry) (\s a -> s {entry = a} :: FailedCreateAssociation)
{-# DEPRECATED fcaEntry "Use generic-lens or generic-optics with 'entry' instead." #-}

-- | The source of the failure.
--
-- /Note:/ Consider using 'fault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcaFault :: Lens.Lens' FailedCreateAssociation (Lude.Maybe Fault)
fcaFault = Lens.lens (fault :: FailedCreateAssociation -> Lude.Maybe Fault) (\s a -> s {fault = a} :: FailedCreateAssociation)
{-# DEPRECATED fcaFault "Use generic-lens or generic-optics with 'fault' instead." #-}

-- | A description of the failure.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcaMessage :: Lens.Lens' FailedCreateAssociation (Lude.Maybe Lude.Text)
fcaMessage = Lens.lens (message :: FailedCreateAssociation -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: FailedCreateAssociation)
{-# DEPRECATED fcaMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON FailedCreateAssociation where
  parseJSON =
    Lude.withObject
      "FailedCreateAssociation"
      ( \x ->
          FailedCreateAssociation'
            Lude.<$> (x Lude..:? "Entry")
            Lude.<*> (x Lude..:? "Fault")
            Lude.<*> (x Lude..:? "Message")
      )
