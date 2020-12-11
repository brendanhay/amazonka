-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterParameterStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterParameterStatus
  ( ClusterParameterStatus (..),

    -- * Smart constructor
    mkClusterParameterStatus,

    -- * Lenses
    cpsParameterApplyErrorDescription,
    cpsParameterName,
    cpsParameterApplyStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Describes the status of a parameter group.
--
-- /See:/ 'mkClusterParameterStatus' smart constructor.
data ClusterParameterStatus = ClusterParameterStatus'
  { parameterApplyErrorDescription ::
      Lude.Maybe Lude.Text,
    parameterName :: Lude.Maybe Lude.Text,
    parameterApplyStatus :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClusterParameterStatus' with the minimum fields required to make a request.
--
-- * 'parameterApplyErrorDescription' - The error that prevented the parameter from being applied to the database.
-- * 'parameterApplyStatus' - The status of the parameter that indicates whether the parameter is in sync with the database, waiting for a cluster reboot, or encountered an error when being applied.
--
-- The following are possible statuses and descriptions.
--
--     * @in-sync@ : The parameter value is in sync with the database.
--
--
--     * @pending-reboot@ : The parameter value will be applied after the cluster reboots.
--
--
--     * @applying@ : The parameter value is being applied to the database.
--
--
--     * @invalid-parameter@ : Cannot apply the parameter value because it has an invalid value or syntax.
--
--
--     * @apply-deferred@ : The parameter contains static property changes. The changes are deferred until the cluster reboots.
--
--
--     * @apply-error@ : Cannot connect to the cluster. The parameter change will be applied after the cluster reboots.
--
--
--     * @unknown-error@ : Cannot apply the parameter change right now. The change will be applied after the cluster reboots.
--
--
-- * 'parameterName' - The name of the parameter.
mkClusterParameterStatus ::
  ClusterParameterStatus
mkClusterParameterStatus =
  ClusterParameterStatus'
    { parameterApplyErrorDescription =
        Lude.Nothing,
      parameterName = Lude.Nothing,
      parameterApplyStatus = Lude.Nothing
    }

-- | The error that prevented the parameter from being applied to the database.
--
-- /Note:/ Consider using 'parameterApplyErrorDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsParameterApplyErrorDescription :: Lens.Lens' ClusterParameterStatus (Lude.Maybe Lude.Text)
cpsParameterApplyErrorDescription = Lens.lens (parameterApplyErrorDescription :: ClusterParameterStatus -> Lude.Maybe Lude.Text) (\s a -> s {parameterApplyErrorDescription = a} :: ClusterParameterStatus)
{-# DEPRECATED cpsParameterApplyErrorDescription "Use generic-lens or generic-optics with 'parameterApplyErrorDescription' instead." #-}

-- | The name of the parameter.
--
-- /Note:/ Consider using 'parameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsParameterName :: Lens.Lens' ClusterParameterStatus (Lude.Maybe Lude.Text)
cpsParameterName = Lens.lens (parameterName :: ClusterParameterStatus -> Lude.Maybe Lude.Text) (\s a -> s {parameterName = a} :: ClusterParameterStatus)
{-# DEPRECATED cpsParameterName "Use generic-lens or generic-optics with 'parameterName' instead." #-}

-- | The status of the parameter that indicates whether the parameter is in sync with the database, waiting for a cluster reboot, or encountered an error when being applied.
--
-- The following are possible statuses and descriptions.
--
--     * @in-sync@ : The parameter value is in sync with the database.
--
--
--     * @pending-reboot@ : The parameter value will be applied after the cluster reboots.
--
--
--     * @applying@ : The parameter value is being applied to the database.
--
--
--     * @invalid-parameter@ : Cannot apply the parameter value because it has an invalid value or syntax.
--
--
--     * @apply-deferred@ : The parameter contains static property changes. The changes are deferred until the cluster reboots.
--
--
--     * @apply-error@ : Cannot connect to the cluster. The parameter change will be applied after the cluster reboots.
--
--
--     * @unknown-error@ : Cannot apply the parameter change right now. The change will be applied after the cluster reboots.
--
--
--
-- /Note:/ Consider using 'parameterApplyStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpsParameterApplyStatus :: Lens.Lens' ClusterParameterStatus (Lude.Maybe Lude.Text)
cpsParameterApplyStatus = Lens.lens (parameterApplyStatus :: ClusterParameterStatus -> Lude.Maybe Lude.Text) (\s a -> s {parameterApplyStatus = a} :: ClusterParameterStatus)
{-# DEPRECATED cpsParameterApplyStatus "Use generic-lens or generic-optics with 'parameterApplyStatus' instead." #-}

instance Lude.FromXML ClusterParameterStatus where
  parseXML x =
    ClusterParameterStatus'
      Lude.<$> (x Lude..@? "ParameterApplyErrorDescription")
      Lude.<*> (x Lude..@? "ParameterName")
      Lude.<*> (x Lude..@? "ParameterApplyStatus")
