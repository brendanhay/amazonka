-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.SnapshotErrorMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.SnapshotErrorMessage
  ( SnapshotErrorMessage (..),

    -- * Smart constructor
    mkSnapshotErrorMessage,

    -- * Lenses
    semFailureReason,
    semSnapshotIdentifier,
    semSnapshotClusterIdentifier,
    semFailureCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Describes the errors returned by a snapshot.
--
-- /See:/ 'mkSnapshotErrorMessage' smart constructor.
data SnapshotErrorMessage = SnapshotErrorMessage'
  { failureReason ::
      Lude.Maybe Lude.Text,
    snapshotIdentifier :: Lude.Maybe Lude.Text,
    snapshotClusterIdentifier :: Lude.Maybe Lude.Text,
    failureCode :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SnapshotErrorMessage' with the minimum fields required to make a request.
--
-- * 'failureCode' - The failure code for the error.
-- * 'failureReason' - The text message describing the error.
-- * 'snapshotClusterIdentifier' - A unique identifier for the cluster.
-- * 'snapshotIdentifier' - A unique identifier for the snapshot returning the error.
mkSnapshotErrorMessage ::
  SnapshotErrorMessage
mkSnapshotErrorMessage =
  SnapshotErrorMessage'
    { failureReason = Lude.Nothing,
      snapshotIdentifier = Lude.Nothing,
      snapshotClusterIdentifier = Lude.Nothing,
      failureCode = Lude.Nothing
    }

-- | The text message describing the error.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
semFailureReason :: Lens.Lens' SnapshotErrorMessage (Lude.Maybe Lude.Text)
semFailureReason = Lens.lens (failureReason :: SnapshotErrorMessage -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: SnapshotErrorMessage)
{-# DEPRECATED semFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | A unique identifier for the snapshot returning the error.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
semSnapshotIdentifier :: Lens.Lens' SnapshotErrorMessage (Lude.Maybe Lude.Text)
semSnapshotIdentifier = Lens.lens (snapshotIdentifier :: SnapshotErrorMessage -> Lude.Maybe Lude.Text) (\s a -> s {snapshotIdentifier = a} :: SnapshotErrorMessage)
{-# DEPRECATED semSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

-- | A unique identifier for the cluster.
--
-- /Note:/ Consider using 'snapshotClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
semSnapshotClusterIdentifier :: Lens.Lens' SnapshotErrorMessage (Lude.Maybe Lude.Text)
semSnapshotClusterIdentifier = Lens.lens (snapshotClusterIdentifier :: SnapshotErrorMessage -> Lude.Maybe Lude.Text) (\s a -> s {snapshotClusterIdentifier = a} :: SnapshotErrorMessage)
{-# DEPRECATED semSnapshotClusterIdentifier "Use generic-lens or generic-optics with 'snapshotClusterIdentifier' instead." #-}

-- | The failure code for the error.
--
-- /Note:/ Consider using 'failureCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
semFailureCode :: Lens.Lens' SnapshotErrorMessage (Lude.Maybe Lude.Text)
semFailureCode = Lens.lens (failureCode :: SnapshotErrorMessage -> Lude.Maybe Lude.Text) (\s a -> s {failureCode = a} :: SnapshotErrorMessage)
{-# DEPRECATED semFailureCode "Use generic-lens or generic-optics with 'failureCode' instead." #-}

instance Lude.FromXML SnapshotErrorMessage where
  parseXML x =
    SnapshotErrorMessage'
      Lude.<$> (x Lude..@? "FailureReason")
      Lude.<*> (x Lude..@? "SnapshotIdentifier")
      Lude.<*> (x Lude..@? "SnapshotClusterIdentifier")
      Lude.<*> (x Lude..@? "FailureCode")
