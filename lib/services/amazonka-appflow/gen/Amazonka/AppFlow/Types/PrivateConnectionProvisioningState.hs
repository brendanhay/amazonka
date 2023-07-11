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
-- Module      : Amazonka.AppFlow.Types.PrivateConnectionProvisioningState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.PrivateConnectionProvisioningState where

import Amazonka.AppFlow.Types.PrivateConnectionProvisioningFailureCause
import Amazonka.AppFlow.Types.PrivateConnectionProvisioningStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the private connection provisioning state.
--
-- /See:/ 'newPrivateConnectionProvisioningState' smart constructor.
data PrivateConnectionProvisioningState = PrivateConnectionProvisioningState'
  { -- | Specifies the private connection provisioning failure cause.
    failureCause :: Prelude.Maybe PrivateConnectionProvisioningFailureCause,
    -- | Specifies the private connection provisioning failure reason.
    failureMessage :: Prelude.Maybe Prelude.Text,
    -- | Specifies the private connection provisioning status.
    status :: Prelude.Maybe PrivateConnectionProvisioningStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrivateConnectionProvisioningState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureCause', 'privateConnectionProvisioningState_failureCause' - Specifies the private connection provisioning failure cause.
--
-- 'failureMessage', 'privateConnectionProvisioningState_failureMessage' - Specifies the private connection provisioning failure reason.
--
-- 'status', 'privateConnectionProvisioningState_status' - Specifies the private connection provisioning status.
newPrivateConnectionProvisioningState ::
  PrivateConnectionProvisioningState
newPrivateConnectionProvisioningState =
  PrivateConnectionProvisioningState'
    { failureCause =
        Prelude.Nothing,
      failureMessage = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Specifies the private connection provisioning failure cause.
privateConnectionProvisioningState_failureCause :: Lens.Lens' PrivateConnectionProvisioningState (Prelude.Maybe PrivateConnectionProvisioningFailureCause)
privateConnectionProvisioningState_failureCause = Lens.lens (\PrivateConnectionProvisioningState' {failureCause} -> failureCause) (\s@PrivateConnectionProvisioningState' {} a -> s {failureCause = a} :: PrivateConnectionProvisioningState)

-- | Specifies the private connection provisioning failure reason.
privateConnectionProvisioningState_failureMessage :: Lens.Lens' PrivateConnectionProvisioningState (Prelude.Maybe Prelude.Text)
privateConnectionProvisioningState_failureMessage = Lens.lens (\PrivateConnectionProvisioningState' {failureMessage} -> failureMessage) (\s@PrivateConnectionProvisioningState' {} a -> s {failureMessage = a} :: PrivateConnectionProvisioningState)

-- | Specifies the private connection provisioning status.
privateConnectionProvisioningState_status :: Lens.Lens' PrivateConnectionProvisioningState (Prelude.Maybe PrivateConnectionProvisioningStatus)
privateConnectionProvisioningState_status = Lens.lens (\PrivateConnectionProvisioningState' {status} -> status) (\s@PrivateConnectionProvisioningState' {} a -> s {status = a} :: PrivateConnectionProvisioningState)

instance
  Data.FromJSON
    PrivateConnectionProvisioningState
  where
  parseJSON =
    Data.withObject
      "PrivateConnectionProvisioningState"
      ( \x ->
          PrivateConnectionProvisioningState'
            Prelude.<$> (x Data..:? "failureCause")
            Prelude.<*> (x Data..:? "failureMessage")
            Prelude.<*> (x Data..:? "status")
      )

instance
  Prelude.Hashable
    PrivateConnectionProvisioningState
  where
  hashWithSalt
    _salt
    PrivateConnectionProvisioningState' {..} =
      _salt
        `Prelude.hashWithSalt` failureCause
        `Prelude.hashWithSalt` failureMessage
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    PrivateConnectionProvisioningState
  where
  rnf PrivateConnectionProvisioningState' {..} =
    Prelude.rnf failureCause
      `Prelude.seq` Prelude.rnf failureMessage
      `Prelude.seq` Prelude.rnf status
