{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.UpdateDeviceCertificateParams
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.UpdateDeviceCertificateParams
  ( UpdateDeviceCertificateParams (..),

    -- * Smart constructor
    mkUpdateDeviceCertificateParams,

    -- * Lenses
    udcpAction,
  )
where

import qualified Network.AWS.IoT.Types.DeviceCertificateUpdateAction as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Parameters to define a mitigation action that changes the state of the device certificate to inactive.
--
-- /See:/ 'mkUpdateDeviceCertificateParams' smart constructor.
newtype UpdateDeviceCertificateParams = UpdateDeviceCertificateParams'
  { -- | The action that you want to apply to the device cerrtificate. The only supported value is @DEACTIVATE@ .
    action :: Types.DeviceCertificateUpdateAction
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDeviceCertificateParams' value with any optional fields omitted.
mkUpdateDeviceCertificateParams ::
  -- | 'action'
  Types.DeviceCertificateUpdateAction ->
  UpdateDeviceCertificateParams
mkUpdateDeviceCertificateParams action =
  UpdateDeviceCertificateParams' {action}

-- | The action that you want to apply to the device cerrtificate. The only supported value is @DEACTIVATE@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcpAction :: Lens.Lens' UpdateDeviceCertificateParams Types.DeviceCertificateUpdateAction
udcpAction = Lens.field @"action"
{-# DEPRECATED udcpAction "Use generic-lens or generic-optics with 'action' instead." #-}

instance Core.FromJSON UpdateDeviceCertificateParams where
  toJSON UpdateDeviceCertificateParams {..} =
    Core.object
      (Core.catMaybes [Core.Just ("action" Core..= action)])

instance Core.FromJSON UpdateDeviceCertificateParams where
  parseJSON =
    Core.withObject "UpdateDeviceCertificateParams" Core.$
      \x -> UpdateDeviceCertificateParams' Core.<$> (x Core..: "action")
