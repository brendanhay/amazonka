-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ResumeClusterMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ResumeClusterMessage
  ( ResumeClusterMessage (..),

    -- * Smart constructor
    mkResumeClusterMessage,

    -- * Lenses
    rClusterIdentifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Describes a resume cluster operation. For example, a scheduled action to run the @ResumeCluster@ API operation.
--
-- /See:/ 'mkResumeClusterMessage' smart constructor.
newtype ResumeClusterMessage = ResumeClusterMessage'
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

-- | Creates a value of 'ResumeClusterMessage' with the minimum fields required to make a request.
--
-- * 'clusterIdentifier' - The identifier of the cluster to be resumed.
mkResumeClusterMessage ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  ResumeClusterMessage
mkResumeClusterMessage pClusterIdentifier_ =
  ResumeClusterMessage' {clusterIdentifier = pClusterIdentifier_}

-- | The identifier of the cluster to be resumed.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rClusterIdentifier :: Lens.Lens' ResumeClusterMessage Lude.Text
rClusterIdentifier = Lens.lens (clusterIdentifier :: ResumeClusterMessage -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: ResumeClusterMessage)
{-# DEPRECATED rClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

instance Lude.FromXML ResumeClusterMessage where
  parseXML x =
    ResumeClusterMessage' Lude.<$> (x Lude..@ "ClusterIdentifier")

instance Lude.ToQuery ResumeClusterMessage where
  toQuery ResumeClusterMessage' {..} =
    Lude.mconcat ["ClusterIdentifier" Lude.=: clusterIdentifier]
