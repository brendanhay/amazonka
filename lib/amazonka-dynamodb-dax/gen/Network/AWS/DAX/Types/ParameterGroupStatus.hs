-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.ParameterGroupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.ParameterGroupStatus
  ( ParameterGroupStatus (..),

    -- * Smart constructor
    mkParameterGroupStatus,

    -- * Lenses
    pgsNodeIdsToReboot,
    pgsParameterApplyStatus,
    pgsParameterGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The status of a parameter group.
--
-- /See:/ 'mkParameterGroupStatus' smart constructor.
data ParameterGroupStatus = ParameterGroupStatus'
  { nodeIdsToReboot ::
      Lude.Maybe [Lude.Text],
    parameterApplyStatus :: Lude.Maybe Lude.Text,
    parameterGroupName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ParameterGroupStatus' with the minimum fields required to make a request.
--
-- * 'nodeIdsToReboot' - The node IDs of one or more nodes to be rebooted.
-- * 'parameterApplyStatus' - The status of parameter updates.
-- * 'parameterGroupName' - The name of the parameter group.
mkParameterGroupStatus ::
  ParameterGroupStatus
mkParameterGroupStatus =
  ParameterGroupStatus'
    { nodeIdsToReboot = Lude.Nothing,
      parameterApplyStatus = Lude.Nothing,
      parameterGroupName = Lude.Nothing
    }

-- | The node IDs of one or more nodes to be rebooted.
--
-- /Note:/ Consider using 'nodeIdsToReboot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgsNodeIdsToReboot :: Lens.Lens' ParameterGroupStatus (Lude.Maybe [Lude.Text])
pgsNodeIdsToReboot = Lens.lens (nodeIdsToReboot :: ParameterGroupStatus -> Lude.Maybe [Lude.Text]) (\s a -> s {nodeIdsToReboot = a} :: ParameterGroupStatus)
{-# DEPRECATED pgsNodeIdsToReboot "Use generic-lens or generic-optics with 'nodeIdsToReboot' instead." #-}

-- | The status of parameter updates.
--
-- /Note:/ Consider using 'parameterApplyStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgsParameterApplyStatus :: Lens.Lens' ParameterGroupStatus (Lude.Maybe Lude.Text)
pgsParameterApplyStatus = Lens.lens (parameterApplyStatus :: ParameterGroupStatus -> Lude.Maybe Lude.Text) (\s a -> s {parameterApplyStatus = a} :: ParameterGroupStatus)
{-# DEPRECATED pgsParameterApplyStatus "Use generic-lens or generic-optics with 'parameterApplyStatus' instead." #-}

-- | The name of the parameter group.
--
-- /Note:/ Consider using 'parameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgsParameterGroupName :: Lens.Lens' ParameterGroupStatus (Lude.Maybe Lude.Text)
pgsParameterGroupName = Lens.lens (parameterGroupName :: ParameterGroupStatus -> Lude.Maybe Lude.Text) (\s a -> s {parameterGroupName = a} :: ParameterGroupStatus)
{-# DEPRECATED pgsParameterGroupName "Use generic-lens or generic-optics with 'parameterGroupName' instead." #-}

instance Lude.FromJSON ParameterGroupStatus where
  parseJSON =
    Lude.withObject
      "ParameterGroupStatus"
      ( \x ->
          ParameterGroupStatus'
            Lude.<$> (x Lude..:? "NodeIdsToReboot" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ParameterApplyStatus")
            Lude.<*> (x Lude..:? "ParameterGroupName")
      )
