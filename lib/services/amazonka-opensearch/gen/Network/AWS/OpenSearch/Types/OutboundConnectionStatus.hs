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
-- Module      : Network.AWS.OpenSearch.Types.OutboundConnectionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpenSearch.Types.OutboundConnectionStatus where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpenSearch.Types.OutboundConnectionStatusCode
import qualified Network.AWS.Prelude as Prelude

-- | The connection status of an outbound cross-cluster connection.
--
-- /See:/ 'newOutboundConnectionStatus' smart constructor.
data OutboundConnectionStatus = OutboundConnectionStatus'
  { -- | Verbose information for the outbound connection status.
    message :: Prelude.Maybe Prelude.Text,
    -- | The state code for the outbound connection. Can be one of the following:
    --
    -- -   VALIDATING: The outbound connection request is being validated.
    -- -   VALIDATION_FAILED: Validation failed for the connection request.
    -- -   PENDING_ACCEPTANCE: Outbound connection request is validated and is
    --     not yet accepted by the remote domain owner.
    -- -   APPROVED: Outbound connection has been approved by the remote domain
    --     owner for getting provisioned.
    -- -   PROVISIONING: Outbound connection request is in process.
    -- -   ACTIVE: Outbound connection is active and ready to use.
    -- -   REJECTING: Outbound connection rejection by remote domain owner is
    --     in progress.
    -- -   REJECTED: Outbound connection request is rejected by remote domain
    --     owner.
    -- -   DELETING: Outbound connection deletion is in progress.
    -- -   DELETED: Outbound connection is deleted and can no longer be used.
    statusCode :: Prelude.Maybe OutboundConnectionStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutboundConnectionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'outboundConnectionStatus_message' - Verbose information for the outbound connection status.
--
-- 'statusCode', 'outboundConnectionStatus_statusCode' - The state code for the outbound connection. Can be one of the following:
--
-- -   VALIDATING: The outbound connection request is being validated.
-- -   VALIDATION_FAILED: Validation failed for the connection request.
-- -   PENDING_ACCEPTANCE: Outbound connection request is validated and is
--     not yet accepted by the remote domain owner.
-- -   APPROVED: Outbound connection has been approved by the remote domain
--     owner for getting provisioned.
-- -   PROVISIONING: Outbound connection request is in process.
-- -   ACTIVE: Outbound connection is active and ready to use.
-- -   REJECTING: Outbound connection rejection by remote domain owner is
--     in progress.
-- -   REJECTED: Outbound connection request is rejected by remote domain
--     owner.
-- -   DELETING: Outbound connection deletion is in progress.
-- -   DELETED: Outbound connection is deleted and can no longer be used.
newOutboundConnectionStatus ::
  OutboundConnectionStatus
newOutboundConnectionStatus =
  OutboundConnectionStatus'
    { message =
        Prelude.Nothing,
      statusCode = Prelude.Nothing
    }

-- | Verbose information for the outbound connection status.
outboundConnectionStatus_message :: Lens.Lens' OutboundConnectionStatus (Prelude.Maybe Prelude.Text)
outboundConnectionStatus_message = Lens.lens (\OutboundConnectionStatus' {message} -> message) (\s@OutboundConnectionStatus' {} a -> s {message = a} :: OutboundConnectionStatus)

-- | The state code for the outbound connection. Can be one of the following:
--
-- -   VALIDATING: The outbound connection request is being validated.
-- -   VALIDATION_FAILED: Validation failed for the connection request.
-- -   PENDING_ACCEPTANCE: Outbound connection request is validated and is
--     not yet accepted by the remote domain owner.
-- -   APPROVED: Outbound connection has been approved by the remote domain
--     owner for getting provisioned.
-- -   PROVISIONING: Outbound connection request is in process.
-- -   ACTIVE: Outbound connection is active and ready to use.
-- -   REJECTING: Outbound connection rejection by remote domain owner is
--     in progress.
-- -   REJECTED: Outbound connection request is rejected by remote domain
--     owner.
-- -   DELETING: Outbound connection deletion is in progress.
-- -   DELETED: Outbound connection is deleted and can no longer be used.
outboundConnectionStatus_statusCode :: Lens.Lens' OutboundConnectionStatus (Prelude.Maybe OutboundConnectionStatusCode)
outboundConnectionStatus_statusCode = Lens.lens (\OutboundConnectionStatus' {statusCode} -> statusCode) (\s@OutboundConnectionStatus' {} a -> s {statusCode = a} :: OutboundConnectionStatus)

instance Core.FromJSON OutboundConnectionStatus where
  parseJSON =
    Core.withObject
      "OutboundConnectionStatus"
      ( \x ->
          OutboundConnectionStatus'
            Prelude.<$> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "StatusCode")
      )

instance Prelude.Hashable OutboundConnectionStatus

instance Prelude.NFData OutboundConnectionStatus
