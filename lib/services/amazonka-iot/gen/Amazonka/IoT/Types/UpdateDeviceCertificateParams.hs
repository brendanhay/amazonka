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
-- Module      : Amazonka.IoT.Types.UpdateDeviceCertificateParams
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.UpdateDeviceCertificateParams where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.DeviceCertificateUpdateAction
import qualified Amazonka.Prelude as Prelude

-- | Parameters to define a mitigation action that changes the state of the
-- device certificate to inactive.
--
-- /See:/ 'newUpdateDeviceCertificateParams' smart constructor.
data UpdateDeviceCertificateParams = UpdateDeviceCertificateParams'
  { -- | The action that you want to apply to the device certificate. The only
    -- supported value is @DEACTIVATE@.
    action :: DeviceCertificateUpdateAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON UpdateDeviceCertificateParams where
  parseJSON =
    Data.withObject
      "UpdateDeviceCertificateParams"
      ( \x ->
          UpdateDeviceCertificateParams'
            Prelude.<$> (x Data..: "action")
      )

instance
  Prelude.Hashable
    UpdateDeviceCertificateParams
  where
  hashWithSalt _salt UpdateDeviceCertificateParams' {..} =
    _salt `Prelude.hashWithSalt` action

instance Prelude.NFData UpdateDeviceCertificateParams where
  rnf UpdateDeviceCertificateParams' {..} =
    Prelude.rnf action

instance Data.ToJSON UpdateDeviceCertificateParams where
  toJSON UpdateDeviceCertificateParams' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("action" Data..= action)]
      )
