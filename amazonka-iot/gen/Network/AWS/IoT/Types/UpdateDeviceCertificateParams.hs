{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.UpdateDeviceCertificateParams
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.UpdateDeviceCertificateParams where

import Network.AWS.IoT.Types.DeviceCertificateUpdateAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Parameters to define a mitigation action that changes the state of the
-- device certificate to inactive.
--
-- /See:/ 'newUpdateDeviceCertificateParams' smart constructor.
data UpdateDeviceCertificateParams = UpdateDeviceCertificateParams'
  { -- | The action that you want to apply to the device certificate. The only
    -- supported value is @DEACTIVATE@.
    action :: DeviceCertificateUpdateAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateDeviceCertificateParams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'updateDeviceCertificateParams_action' - The action that you want to apply to the device certificate. The only
-- supported value is @DEACTIVATE@.
newUpdateDeviceCertificateParams ::
  -- | 'action'
  DeviceCertificateUpdateAction ->
  UpdateDeviceCertificateParams
newUpdateDeviceCertificateParams pAction_ =
  UpdateDeviceCertificateParams' {action = pAction_}

-- | The action that you want to apply to the device certificate. The only
-- supported value is @DEACTIVATE@.
updateDeviceCertificateParams_action :: Lens.Lens' UpdateDeviceCertificateParams DeviceCertificateUpdateAction
updateDeviceCertificateParams_action = Lens.lens (\UpdateDeviceCertificateParams' {action} -> action) (\s@UpdateDeviceCertificateParams' {} a -> s {action = a} :: UpdateDeviceCertificateParams)

instance
  Prelude.FromJSON
    UpdateDeviceCertificateParams
  where
  parseJSON =
    Prelude.withObject
      "UpdateDeviceCertificateParams"
      ( \x ->
          UpdateDeviceCertificateParams'
            Prelude.<$> (x Prelude..: "action")
      )

instance
  Prelude.Hashable
    UpdateDeviceCertificateParams

instance Prelude.NFData UpdateDeviceCertificateParams

instance Prelude.ToJSON UpdateDeviceCertificateParams where
  toJSON UpdateDeviceCertificateParams' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("action" Prelude..= action)]
      )
