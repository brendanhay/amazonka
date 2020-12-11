-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceExportDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceExportDetails
  ( InstanceExportDetails (..),

    -- * Smart constructor
    mkInstanceExportDetails,

    -- * Lenses
    iedTargetEnvironment,
    iedInstanceId,
  )
where

import Network.AWS.EC2.Types.ExportEnvironment
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an instance to export.
--
-- /See:/ 'mkInstanceExportDetails' smart constructor.
data InstanceExportDetails = InstanceExportDetails'
  { targetEnvironment ::
      Lude.Maybe ExportEnvironment,
    instanceId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceExportDetails' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the resource being exported.
-- * 'targetEnvironment' - The target virtualization environment.
mkInstanceExportDetails ::
  InstanceExportDetails
mkInstanceExportDetails =
  InstanceExportDetails'
    { targetEnvironment = Lude.Nothing,
      instanceId = Lude.Nothing
    }

-- | The target virtualization environment.
--
-- /Note:/ Consider using 'targetEnvironment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iedTargetEnvironment :: Lens.Lens' InstanceExportDetails (Lude.Maybe ExportEnvironment)
iedTargetEnvironment = Lens.lens (targetEnvironment :: InstanceExportDetails -> Lude.Maybe ExportEnvironment) (\s a -> s {targetEnvironment = a} :: InstanceExportDetails)
{-# DEPRECATED iedTargetEnvironment "Use generic-lens or generic-optics with 'targetEnvironment' instead." #-}

-- | The ID of the resource being exported.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iedInstanceId :: Lens.Lens' InstanceExportDetails (Lude.Maybe Lude.Text)
iedInstanceId = Lens.lens (instanceId :: InstanceExportDetails -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: InstanceExportDetails)
{-# DEPRECATED iedInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.FromXML InstanceExportDetails where
  parseXML x =
    InstanceExportDetails'
      Lude.<$> (x Lude..@? "targetEnvironment") Lude.<*> (x Lude..@? "instanceId")
