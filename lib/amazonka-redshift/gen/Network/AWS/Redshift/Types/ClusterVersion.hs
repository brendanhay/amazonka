{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterVersion
  ( ClusterVersion (..),

    -- * Smart constructor
    mkClusterVersion,

    -- * Lenses
    cvClusterParameterGroupFamily,
    cvClusterVersion,
    cvDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Describes a cluster version, including the parameter group family and description of the version.
--
-- /See:/ 'mkClusterVersion' smart constructor.
data ClusterVersion = ClusterVersion'
  { clusterParameterGroupFamily ::
      Lude.Maybe Lude.Text,
    clusterVersion :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClusterVersion' with the minimum fields required to make a request.
--
-- * 'clusterParameterGroupFamily' - The name of the cluster parameter group family for the cluster.
-- * 'clusterVersion' - The version number used by the cluster.
-- * 'description' - The description of the cluster version.
mkClusterVersion ::
  ClusterVersion
mkClusterVersion =
  ClusterVersion'
    { clusterParameterGroupFamily = Lude.Nothing,
      clusterVersion = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The name of the cluster parameter group family for the cluster.
--
-- /Note:/ Consider using 'clusterParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvClusterParameterGroupFamily :: Lens.Lens' ClusterVersion (Lude.Maybe Lude.Text)
cvClusterParameterGroupFamily = Lens.lens (clusterParameterGroupFamily :: ClusterVersion -> Lude.Maybe Lude.Text) (\s a -> s {clusterParameterGroupFamily = a} :: ClusterVersion)
{-# DEPRECATED cvClusterParameterGroupFamily "Use generic-lens or generic-optics with 'clusterParameterGroupFamily' instead." #-}

-- | The version number used by the cluster.
--
-- /Note:/ Consider using 'clusterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvClusterVersion :: Lens.Lens' ClusterVersion (Lude.Maybe Lude.Text)
cvClusterVersion = Lens.lens (clusterVersion :: ClusterVersion -> Lude.Maybe Lude.Text) (\s a -> s {clusterVersion = a} :: ClusterVersion)
{-# DEPRECATED cvClusterVersion "Use generic-lens or generic-optics with 'clusterVersion' instead." #-}

-- | The description of the cluster version.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvDescription :: Lens.Lens' ClusterVersion (Lude.Maybe Lude.Text)
cvDescription = Lens.lens (description :: ClusterVersion -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ClusterVersion)
{-# DEPRECATED cvDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML ClusterVersion where
  parseXML x =
    ClusterVersion'
      Lude.<$> (x Lude..@? "ClusterParameterGroupFamily")
      Lude.<*> (x Lude..@? "ClusterVersion")
      Lude.<*> (x Lude..@? "Description")
