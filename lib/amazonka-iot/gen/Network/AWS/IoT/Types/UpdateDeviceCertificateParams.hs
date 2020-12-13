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

import Network.AWS.IoT.Types.DeviceCertificateUpdateAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Parameters to define a mitigation action that changes the state of the device certificate to inactive.
--
-- /See:/ 'mkUpdateDeviceCertificateParams' smart constructor.
newtype UpdateDeviceCertificateParams = UpdateDeviceCertificateParams'
  { -- | The action that you want to apply to the device cerrtificate. The only supported value is @DEACTIVATE@ .
    action :: DeviceCertificateUpdateAction
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDeviceCertificateParams' with the minimum fields required to make a request.
--
-- * 'action' - The action that you want to apply to the device cerrtificate. The only supported value is @DEACTIVATE@ .
mkUpdateDeviceCertificateParams ::
  -- | 'action'
  DeviceCertificateUpdateAction ->
  UpdateDeviceCertificateParams
mkUpdateDeviceCertificateParams pAction_ =
  UpdateDeviceCertificateParams' {action = pAction_}

-- | The action that you want to apply to the device cerrtificate. The only supported value is @DEACTIVATE@ .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcpAction :: Lens.Lens' UpdateDeviceCertificateParams DeviceCertificateUpdateAction
udcpAction = Lens.lens (action :: UpdateDeviceCertificateParams -> DeviceCertificateUpdateAction) (\s a -> s {action = a} :: UpdateDeviceCertificateParams)
{-# DEPRECATED udcpAction "Use generic-lens or generic-optics with 'action' instead." #-}

instance Lude.FromJSON UpdateDeviceCertificateParams where
  parseJSON =
    Lude.withObject
      "UpdateDeviceCertificateParams"
      ( \x ->
          UpdateDeviceCertificateParams' Lude.<$> (x Lude..: "action")
      )

instance Lude.ToJSON UpdateDeviceCertificateParams where
  toJSON UpdateDeviceCertificateParams' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("action" Lude..= action)])
