{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.PauseClusterMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.PauseClusterMessage
  ( PauseClusterMessage (..)
  -- * Smart constructor
  , mkPauseClusterMessage
  -- * Lenses
  , pcmClusterIdentifier
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types

-- | Describes a pause cluster operation. For example, a scheduled action to run the @PauseCluster@ API operation. 
--
-- /See:/ 'mkPauseClusterMessage' smart constructor.
newtype PauseClusterMessage = PauseClusterMessage'
  { clusterIdentifier :: Core.Text
    -- ^ The identifier of the cluster to be paused.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PauseClusterMessage' value with any optional fields omitted.
mkPauseClusterMessage
    :: Core.Text -- ^ 'clusterIdentifier'
    -> PauseClusterMessage
mkPauseClusterMessage clusterIdentifier
  = PauseClusterMessage'{clusterIdentifier}

-- | The identifier of the cluster to be paused.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcmClusterIdentifier :: Lens.Lens' PauseClusterMessage Core.Text
pcmClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE pcmClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

instance Core.ToQuery PauseClusterMessage where
        toQuery PauseClusterMessage{..}
          = Core.toQueryPair "ClusterIdentifier" clusterIdentifier

instance Core.FromXML PauseClusterMessage where
        parseXML x
          = PauseClusterMessage' Core.<$> (x Core..@ "ClusterIdentifier")
