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
-- Module      : Network.AWS.AppFlow.Types.PrivateConnectionProvisioningState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppFlow.Types.PrivateConnectionProvisioningState where

import Network.AWS.AppFlow.Types.PrivateConnectionProvisioningFailureCause
import Network.AWS.AppFlow.Types.PrivateConnectionProvisioningStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the private connection provisioning state.
--
-- /See:/ 'newPrivateConnectionProvisioningState' smart constructor.
data PrivateConnectionProvisioningState = PrivateConnectionProvisioningState'
  { -- | Specifies the private connection provisioning status.
    status :: Prelude.Maybe PrivateConnectionProvisioningStatus,
    -- | Specifies the private connection provisioning failure reason.
    failureMessage :: Prelude.Maybe Prelude.Text,
    -- | Specifies the private connection provisioning failure cause.
    failureCause :: Prelude.Maybe PrivateConnectionProvisioningFailureCause
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
-- 'status', 'privateConnectionProvisioningState_status' - Specifies the private connection provisioning status.
--
-- 'failureMessage', 'privateConnectionProvisioningState_failureMessage' - Specifies the private connection provisioning failure reason.
--
-- 'failureCause', 'privateConnectionProvisioningState_failureCause' - Specifies the private connection provisioning failure cause.
newPrivateConnectionProvisioningState ::
  PrivateConnectionProvisioningState
newPrivateConnectionProvisioningState =
  PrivateConnectionProvisioningState'
    { status =
        Prelude.Nothing,
      failureMessage = Prelude.Nothing,
      failureCause = Prelude.Nothing
    }

-- | Specifies the private connection provisioning status.
privateConnectionProvisioningState_status :: Lens.Lens' PrivateConnectionProvisioningState (Prelude.Maybe PrivateConnectionProvisioningStatus)
privateConnectionProvisioningState_status = Lens.lens (\PrivateConnectionProvisioningState' {status} -> status) (\s@PrivateConnectionProvisioningState' {} a -> s {status = a} :: PrivateConnectionProvisioningState)

-- | Specifies the private connection provisioning failure reason.
privateConnectionProvisioningState_failureMessage :: Lens.Lens' PrivateConnectionProvisioningState (Prelude.Maybe Prelude.Text)
privateConnectionProvisioningState_failureMessage = Lens.lens (\PrivateConnectionProvisioningState' {failureMessage} -> failureMessage) (\s@PrivateConnectionProvisioningState' {} a -> s {failureMessage = a} :: PrivateConnectionProvisioningState)

-- | Specifies the private connection provisioning failure cause.
privateConnectionProvisioningState_failureCause :: Lens.Lens' PrivateConnectionProvisioningState (Prelude.Maybe PrivateConnectionProvisioningFailureCause)
privateConnectionProvisioningState_failureCause = Lens.lens (\PrivateConnectionProvisioningState' {failureCause} -> failureCause) (\s@PrivateConnectionProvisioningState' {} a -> s {failureCause = a} :: PrivateConnectionProvisioningState)

instance
  Core.FromJSON
    PrivateConnectionProvisioningState
  where
  parseJSON =
    Core.withObject
      "PrivateConnectionProvisioningState"
      ( \x ->
          PrivateConnectionProvisioningState'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "failureMessage")
            Prelude.<*> (x Core..:? "failureCause")
      )

instance
  Prelude.Hashable
    PrivateConnectionProvisioningState

instance
  Prelude.NFData
    PrivateConnectionProvisioningState
