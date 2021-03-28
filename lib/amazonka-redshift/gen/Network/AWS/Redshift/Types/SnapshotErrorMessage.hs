{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.SnapshotErrorMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.SnapshotErrorMessage
  ( SnapshotErrorMessage (..)
  -- * Smart constructor
  , mkSnapshotErrorMessage
  -- * Lenses
  , semFailureCode
  , semFailureReason
  , semSnapshotClusterIdentifier
  , semSnapshotIdentifier
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types

-- | Describes the errors returned by a snapshot.
--
-- /See:/ 'mkSnapshotErrorMessage' smart constructor.
data SnapshotErrorMessage = SnapshotErrorMessage'
  { failureCode :: Core.Maybe Core.Text
    -- ^ The failure code for the error.
  , failureReason :: Core.Maybe Core.Text
    -- ^ The text message describing the error.
  , snapshotClusterIdentifier :: Core.Maybe Core.Text
    -- ^ A unique identifier for the cluster.
  , snapshotIdentifier :: Core.Maybe Core.Text
    -- ^ A unique identifier for the snapshot returning the error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SnapshotErrorMessage' value with any optional fields omitted.
mkSnapshotErrorMessage
    :: SnapshotErrorMessage
mkSnapshotErrorMessage
  = SnapshotErrorMessage'{failureCode = Core.Nothing,
                          failureReason = Core.Nothing,
                          snapshotClusterIdentifier = Core.Nothing,
                          snapshotIdentifier = Core.Nothing}

-- | The failure code for the error.
--
-- /Note:/ Consider using 'failureCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
semFailureCode :: Lens.Lens' SnapshotErrorMessage (Core.Maybe Core.Text)
semFailureCode = Lens.field @"failureCode"
{-# INLINEABLE semFailureCode #-}
{-# DEPRECATED failureCode "Use generic-lens or generic-optics with 'failureCode' instead"  #-}

-- | The text message describing the error.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
semFailureReason :: Lens.Lens' SnapshotErrorMessage (Core.Maybe Core.Text)
semFailureReason = Lens.field @"failureReason"
{-# INLINEABLE semFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | A unique identifier for the cluster.
--
-- /Note:/ Consider using 'snapshotClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
semSnapshotClusterIdentifier :: Lens.Lens' SnapshotErrorMessage (Core.Maybe Core.Text)
semSnapshotClusterIdentifier = Lens.field @"snapshotClusterIdentifier"
{-# INLINEABLE semSnapshotClusterIdentifier #-}
{-# DEPRECATED snapshotClusterIdentifier "Use generic-lens or generic-optics with 'snapshotClusterIdentifier' instead"  #-}

-- | A unique identifier for the snapshot returning the error.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
semSnapshotIdentifier :: Lens.Lens' SnapshotErrorMessage (Core.Maybe Core.Text)
semSnapshotIdentifier = Lens.field @"snapshotIdentifier"
{-# INLINEABLE semSnapshotIdentifier #-}
{-# DEPRECATED snapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead"  #-}

instance Core.FromXML SnapshotErrorMessage where
        parseXML x
          = SnapshotErrorMessage' Core.<$>
              (x Core..@? "FailureCode") Core.<*> x Core..@? "FailureReason"
                Core.<*> x Core..@? "SnapshotClusterIdentifier"
                Core.<*> x Core..@? "SnapshotIdentifier"
