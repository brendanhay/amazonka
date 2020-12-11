-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterParameterGroupNameMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterParameterGroupNameMessage
  ( ClusterParameterGroupNameMessage (..),

    -- * Smart constructor
    mkClusterParameterGroupNameMessage,

    -- * Lenses
    cpgnmParameterGroupStatus,
    cpgnmParameterGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- |
--
-- /See:/ 'mkClusterParameterGroupNameMessage' smart constructor.
data ClusterParameterGroupNameMessage = ClusterParameterGroupNameMessage'
  { parameterGroupStatus ::
      Lude.Maybe Lude.Text,
    parameterGroupName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClusterParameterGroupNameMessage' with the minimum fields required to make a request.
--
-- * 'parameterGroupName' - The name of the cluster parameter group.
-- * 'parameterGroupStatus' - The status of the parameter group. For example, if you made a change to a parameter group name-value pair, then the change could be pending a reboot of an associated cluster.
mkClusterParameterGroupNameMessage ::
  ClusterParameterGroupNameMessage
mkClusterParameterGroupNameMessage =
  ClusterParameterGroupNameMessage'
    { parameterGroupStatus =
        Lude.Nothing,
      parameterGroupName = Lude.Nothing
    }

-- | The status of the parameter group. For example, if you made a change to a parameter group name-value pair, then the change could be pending a reboot of an associated cluster.
--
-- /Note:/ Consider using 'parameterGroupStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgnmParameterGroupStatus :: Lens.Lens' ClusterParameterGroupNameMessage (Lude.Maybe Lude.Text)
cpgnmParameterGroupStatus = Lens.lens (parameterGroupStatus :: ClusterParameterGroupNameMessage -> Lude.Maybe Lude.Text) (\s a -> s {parameterGroupStatus = a} :: ClusterParameterGroupNameMessage)
{-# DEPRECATED cpgnmParameterGroupStatus "Use generic-lens or generic-optics with 'parameterGroupStatus' instead." #-}

-- | The name of the cluster parameter group.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpgnmParameterGroupName :: Lens.Lens' ClusterParameterGroupNameMessage (Lude.Maybe Lude.Text)
cpgnmParameterGroupName = Lens.lens (parameterGroupName :: ClusterParameterGroupNameMessage -> Lude.Maybe Lude.Text) (\s a -> s {parameterGroupName = a} :: ClusterParameterGroupNameMessage)
{-# DEPRECATED cpgnmParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

instance Lude.FromXML ClusterParameterGroupNameMessage where
  parseXML x =
    ClusterParameterGroupNameMessage'
      Lude.<$> (x Lude..@? "ParameterGroupStatus")
      Lude.<*> (x Lude..@? "ParameterGroupName")
