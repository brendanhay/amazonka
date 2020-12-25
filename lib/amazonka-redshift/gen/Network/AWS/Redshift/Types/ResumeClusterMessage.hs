{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.String as Types

-- | Describes a resume cluster operation. For example, a scheduled action to run the @ResumeCluster@ API operation.
--
-- /See:/ 'mkResumeClusterMessage' smart constructor.
newtype ResumeClusterMessage = ResumeClusterMessage'
  { -- | The identifier of the cluster to be resumed.
    clusterIdentifier :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ResumeClusterMessage' value with any optional fields omitted.
mkResumeClusterMessage ::
  -- | 'clusterIdentifier'
  Types.String ->
  ResumeClusterMessage
mkResumeClusterMessage clusterIdentifier =
  ResumeClusterMessage' {clusterIdentifier}

-- | The identifier of the cluster to be resumed.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rClusterIdentifier :: Lens.Lens' ResumeClusterMessage Types.String
rClusterIdentifier = Lens.field @"clusterIdentifier"
{-# DEPRECATED rClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

instance Core.FromXML ResumeClusterMessage where
  parseXML x =
    ResumeClusterMessage' Core.<$> (x Core..@ "ClusterIdentifier")
