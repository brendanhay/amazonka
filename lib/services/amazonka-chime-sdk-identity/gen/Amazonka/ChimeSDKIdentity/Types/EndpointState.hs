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
-- Module      : Amazonka.ChimeSDKIdentity.Types.EndpointState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKIdentity.Types.EndpointState where

import Amazonka.ChimeSDKIdentity.Types.EndpointStatus
import Amazonka.ChimeSDKIdentity.Types.EndpointStatusReason
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A read-only field that represents the state of an
-- @AppInstanceUserEndpoint@. Supported values:
--
-- -   @ACTIVE@: The @AppInstanceUserEndpoint@ is active and able to
--     receive messages. When @ACTIVE@, the @EndpointStatusReason@ remains
--     empty.
--
-- -   @INACTIVE@: The @AppInstanceUserEndpoint@ is inactive and can\'t
--     receive message. When INACTIVE, the corresponding reason will be
--     conveyed through EndpointStatusReason.
--
-- -   @INVALID_DEVICE_TOKEN@ indicates that an @AppInstanceUserEndpoint@
--     is @INACTIVE@ due to invalid device token
--
-- -   @INVALID_PINPOINT_ARN@ indicates that an @AppInstanceUserEndpoint@
--     is @INACTIVE@ due to an invalid pinpoint ARN that was input through
--     the @ResourceArn@ field.
--
-- /See:/ 'newEndpointState' smart constructor.
data EndpointState = EndpointState'
  { -- | The reason for the @EndpointStatus@.
    statusReason :: Prelude.Maybe EndpointStatusReason,
    -- | Enum that indicates the Status of an @AppInstanceUserEndpoint@.
    status :: EndpointStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusReason', 'endpointState_statusReason' - The reason for the @EndpointStatus@.
--
-- 'status', 'endpointState_status' - Enum that indicates the Status of an @AppInstanceUserEndpoint@.
newEndpointState ::
  -- | 'status'
  EndpointStatus ->
  EndpointState
newEndpointState pStatus_ =
  EndpointState'
    { statusReason = Prelude.Nothing,
      status = pStatus_
    }

-- | The reason for the @EndpointStatus@.
endpointState_statusReason :: Lens.Lens' EndpointState (Prelude.Maybe EndpointStatusReason)
endpointState_statusReason = Lens.lens (\EndpointState' {statusReason} -> statusReason) (\s@EndpointState' {} a -> s {statusReason = a} :: EndpointState)

-- | Enum that indicates the Status of an @AppInstanceUserEndpoint@.
endpointState_status :: Lens.Lens' EndpointState EndpointStatus
endpointState_status = Lens.lens (\EndpointState' {status} -> status) (\s@EndpointState' {} a -> s {status = a} :: EndpointState)

instance Core.FromJSON EndpointState where
  parseJSON =
    Core.withObject
      "EndpointState"
      ( \x ->
          EndpointState'
            Prelude.<$> (x Core..:? "StatusReason")
            Prelude.<*> (x Core..: "Status")
      )

instance Prelude.Hashable EndpointState where
  hashWithSalt _salt EndpointState' {..} =
    _salt `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` status

instance Prelude.NFData EndpointState where
  rnf EndpointState' {..} =
    Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf status
