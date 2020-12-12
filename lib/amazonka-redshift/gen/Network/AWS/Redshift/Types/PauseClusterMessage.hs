{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.PauseClusterMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.PauseClusterMessage
  ( PauseClusterMessage (..),

    -- * Smart constructor
    mkPauseClusterMessage,

    -- * Lenses
    pcmClusterIdentifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Describes a pause cluster operation. For example, a scheduled action to run the @PauseCluster@ API operation.
--
-- /See:/ 'mkPauseClusterMessage' smart constructor.
newtype PauseClusterMessage = PauseClusterMessage'
  { clusterIdentifier ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PauseClusterMessage' with the minimum fields required to make a request.
--
-- * 'clusterIdentifier' - The identifier of the cluster to be paused.
mkPauseClusterMessage ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  PauseClusterMessage
mkPauseClusterMessage pClusterIdentifier_ =
  PauseClusterMessage' {clusterIdentifier = pClusterIdentifier_}

-- | The identifier of the cluster to be paused.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcmClusterIdentifier :: Lens.Lens' PauseClusterMessage Lude.Text
pcmClusterIdentifier = Lens.lens (clusterIdentifier :: PauseClusterMessage -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: PauseClusterMessage)
{-# DEPRECATED pcmClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

instance Lude.FromXML PauseClusterMessage where
  parseXML x =
    PauseClusterMessage' Lude.<$> (x Lude..@ "ClusterIdentifier")

instance Lude.ToQuery PauseClusterMessage where
  toQuery PauseClusterMessage' {..} =
    Lude.mconcat ["ClusterIdentifier" Lude.=: clusterIdentifier]
