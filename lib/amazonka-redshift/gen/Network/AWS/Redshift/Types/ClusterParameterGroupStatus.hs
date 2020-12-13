{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterParameterGroupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterParameterGroupStatus
  ( ClusterParameterGroupStatus (..),

    -- * Smart constructor
    mkClusterParameterGroupStatus,

    -- * Lenses
    cpgsClusterParameterStatusList,
    cpgsParameterApplyStatus,
    cpgsParameterGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.ClusterParameterStatus

-- | Describes the status of a parameter group.
--
-- /See:/ 'mkClusterParameterGroupStatus' smart constructor.
data ClusterParameterGroupStatus = ClusterParameterGroupStatus'
  { -- | The list of parameter statuses.
    --
    -- For more information about parameters and parameter groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
    clusterParameterStatusList :: Lude.Maybe [ClusterParameterStatus],
    -- | The status of parameter updates.
    parameterApplyStatus :: Lude.Maybe Lude.Text,
    -- | The name of the cluster parameter group.
    parameterGroupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClusterParameterGroupStatus' with the minimum fields required to make a request.
--
-- * 'clusterParameterStatusList' - The list of parameter statuses.
--
-- For more information about parameters and parameter groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
-- * 'parameterApplyStatus' - The status of parameter updates.
-- * 'parameterGroupName' - The name of the cluster parameter group.
mkClusterParameterGroupStatus ::
  ClusterParameterGroupStatus
mkClusterParameterGroupStatus =
  ClusterParameterGroupStatus'
    { clusterParameterStatusList =
        Lude.Nothing,
      parameterApplyStatus = Lude.Nothing,
      parameterGroupName = Lude.Nothing
    }

-- | The list of parameter statuses.
--
-- For more information about parameters and parameter groups, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
--
-- /Note:/ Consider using 'clusterParameterStatusList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgsClusterParameterStatusList :: Lens.Lens' ClusterParameterGroupStatus (Lude.Maybe [ClusterParameterStatus])
cpgsClusterParameterStatusList = Lens.lens (clusterParameterStatusList :: ClusterParameterGroupStatus -> Lude.Maybe [ClusterParameterStatus]) (\s a -> s {clusterParameterStatusList = a} :: ClusterParameterGroupStatus)
{-# DEPRECATED cpgsClusterParameterStatusList "Use generic-lens or generic-optics with 'clusterParameterStatusList' instead." #-}

-- | The status of parameter updates.
--
-- /Note:/ Consider using 'parameterApplyStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgsParameterApplyStatus :: Lens.Lens' ClusterParameterGroupStatus (Lude.Maybe Lude.Text)
cpgsParameterApplyStatus = Lens.lens (parameterApplyStatus :: ClusterParameterGroupStatus -> Lude.Maybe Lude.Text) (\s a -> s {parameterApplyStatus = a} :: ClusterParameterGroupStatus)
{-# DEPRECATED cpgsParameterApplyStatus "Use generic-lens or generic-optics with 'parameterApplyStatus' instead." #-}

-- | The name of the cluster parameter group.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgsParameterGroupName :: Lens.Lens' ClusterParameterGroupStatus (Lude.Maybe Lude.Text)
cpgsParameterGroupName = Lens.lens (parameterGroupName :: ClusterParameterGroupStatus -> Lude.Maybe Lude.Text) (\s a -> s {parameterGroupName = a} :: ClusterParameterGroupStatus)
{-# DEPRECATED cpgsParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

instance Lude.FromXML ClusterParameterGroupStatus where
  parseXML x =
    ClusterParameterGroupStatus'
      Lude.<$> ( x Lude..@? "ClusterParameterStatusList" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "ParameterApplyStatus")
      Lude.<*> (x Lude..@? "ParameterGroupName")
