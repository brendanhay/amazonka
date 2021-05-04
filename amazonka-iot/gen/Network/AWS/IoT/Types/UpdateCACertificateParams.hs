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
-- Module      : Network.AWS.IoT.Types.UpdateCACertificateParams
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.UpdateCACertificateParams where

import Network.AWS.IoT.Types.CACertificateUpdateAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Parameters to define a mitigation action that changes the state of the
-- CA certificate to inactive.
--
-- /See:/ 'newUpdateCACertificateParams' smart constructor.
data UpdateCACertificateParams = UpdateCACertificateParams'
  { -- | The action that you want to apply to the CA certificate. The only
    -- supported value is @DEACTIVATE@.
    action :: CACertificateUpdateAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateCACertificateParams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'updateCACertificateParams_action' - The action that you want to apply to the CA certificate. The only
-- supported value is @DEACTIVATE@.
newUpdateCACertificateParams ::
  -- | 'action'
  CACertificateUpdateAction ->
  UpdateCACertificateParams
newUpdateCACertificateParams pAction_ =
  UpdateCACertificateParams' {action = pAction_}

-- | The action that you want to apply to the CA certificate. The only
-- supported value is @DEACTIVATE@.
updateCACertificateParams_action :: Lens.Lens' UpdateCACertificateParams CACertificateUpdateAction
updateCACertificateParams_action = Lens.lens (\UpdateCACertificateParams' {action} -> action) (\s@UpdateCACertificateParams' {} a -> s {action = a} :: UpdateCACertificateParams)

instance Prelude.FromJSON UpdateCACertificateParams where
  parseJSON =
    Prelude.withObject
      "UpdateCACertificateParams"
      ( \x ->
          UpdateCACertificateParams'
            Prelude.<$> (x Prelude..: "action")
      )

instance Prelude.Hashable UpdateCACertificateParams

instance Prelude.NFData UpdateCACertificateParams

instance Prelude.ToJSON UpdateCACertificateParams where
  toJSON UpdateCACertificateParams' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("action" Prelude..= action)]
      )
