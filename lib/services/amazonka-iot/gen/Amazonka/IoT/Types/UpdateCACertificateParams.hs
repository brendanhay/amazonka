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
-- Module      : Amazonka.IoT.Types.UpdateCACertificateParams
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.UpdateCACertificateParams where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.CACertificateUpdateAction
import qualified Amazonka.Prelude as Prelude

-- | Parameters to define a mitigation action that changes the state of the
-- CA certificate to inactive.
--
-- /See:/ 'newUpdateCACertificateParams' smart constructor.
data UpdateCACertificateParams = UpdateCACertificateParams'
  { -- | The action that you want to apply to the CA certificate. The only
    -- supported value is @DEACTIVATE@.
    action :: CACertificateUpdateAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON UpdateCACertificateParams where
  parseJSON =
    Data.withObject
      "UpdateCACertificateParams"
      ( \x ->
          UpdateCACertificateParams'
            Prelude.<$> (x Data..: "action")
      )

instance Prelude.Hashable UpdateCACertificateParams where
  hashWithSalt _salt UpdateCACertificateParams' {..} =
    _salt `Prelude.hashWithSalt` action

instance Prelude.NFData UpdateCACertificateParams where
  rnf UpdateCACertificateParams' {..} =
    Prelude.rnf action

instance Data.ToJSON UpdateCACertificateParams where
  toJSON UpdateCACertificateParams' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("action" Data..= action)]
      )
