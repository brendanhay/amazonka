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
-- Module      : Amazonka.OpenSearch.Types.OutboundConnectionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.OutboundConnectionStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.OutboundConnectionStatusCode
import qualified Amazonka.Prelude as Prelude

-- | The status of an outbound cross-cluster connection.
--
-- /See:/ 'newOutboundConnectionStatus' smart constructor.
data OutboundConnectionStatus = OutboundConnectionStatus'
  { -- | Verbose information for the outbound connection.
    message :: Prelude.Maybe Prelude.Text,
    -- | The status code for the outbound connection. Can be one of the
    -- following:
    --
    -- -   __VALIDATING__ - The outbound connection request is being validated.
    --
    -- -   __VALIDATION_FAILED__ - Validation failed for the connection
    --     request.
    --
    -- -   __PENDING_ACCEPTANCE__: Outbound connection request is validated and
    --     is not yet accepted by the remote domain owner.
    --
    -- -   __APPROVED__ - Outbound connection has been approved by the remote
    --     domain owner for getting provisioned.
    --
    -- -   __PROVISIONING__ - Outbound connection request is in process.
    --
    -- -   __ACTIVE__ - Outbound connection is active and ready to use.
    --
    -- -   __REJECTING__ - Outbound connection rejection by remote domain owner
    --     is in progress.
    --
    -- -   __REJECTED__ - Outbound connection request is rejected by remote
    --     domain owner.
    --
    -- -   __DELETING__ - Outbound connection deletion is in progress.
    --
    -- -   __DELETED__ - Outbound connection is deleted and can no longer be
    --     used.
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
-- 'message', 'outboundConnectionStatus_message' - Verbose information for the outbound connection.
--
-- 'statusCode', 'outboundConnectionStatus_statusCode' - The status code for the outbound connection. Can be one of the
-- following:
--
-- -   __VALIDATING__ - The outbound connection request is being validated.
--
-- -   __VALIDATION_FAILED__ - Validation failed for the connection
--     request.
--
-- -   __PENDING_ACCEPTANCE__: Outbound connection request is validated and
--     is not yet accepted by the remote domain owner.
--
-- -   __APPROVED__ - Outbound connection has been approved by the remote
--     domain owner for getting provisioned.
--
-- -   __PROVISIONING__ - Outbound connection request is in process.
--
-- -   __ACTIVE__ - Outbound connection is active and ready to use.
--
-- -   __REJECTING__ - Outbound connection rejection by remote domain owner
--     is in progress.
--
-- -   __REJECTED__ - Outbound connection request is rejected by remote
--     domain owner.
--
-- -   __DELETING__ - Outbound connection deletion is in progress.
--
-- -   __DELETED__ - Outbound connection is deleted and can no longer be
--     used.
newOutboundConnectionStatus ::
  OutboundConnectionStatus
newOutboundConnectionStatus =
  OutboundConnectionStatus'
    { message =
        Prelude.Nothing,
      statusCode = Prelude.Nothing
    }

-- | Verbose information for the outbound connection.
outboundConnectionStatus_message :: Lens.Lens' OutboundConnectionStatus (Prelude.Maybe Prelude.Text)
outboundConnectionStatus_message = Lens.lens (\OutboundConnectionStatus' {message} -> message) (\s@OutboundConnectionStatus' {} a -> s {message = a} :: OutboundConnectionStatus)

-- | The status code for the outbound connection. Can be one of the
-- following:
--
-- -   __VALIDATING__ - The outbound connection request is being validated.
--
-- -   __VALIDATION_FAILED__ - Validation failed for the connection
--     request.
--
-- -   __PENDING_ACCEPTANCE__: Outbound connection request is validated and
--     is not yet accepted by the remote domain owner.
--
-- -   __APPROVED__ - Outbound connection has been approved by the remote
--     domain owner for getting provisioned.
--
-- -   __PROVISIONING__ - Outbound connection request is in process.
--
-- -   __ACTIVE__ - Outbound connection is active and ready to use.
--
-- -   __REJECTING__ - Outbound connection rejection by remote domain owner
--     is in progress.
--
-- -   __REJECTED__ - Outbound connection request is rejected by remote
--     domain owner.
--
-- -   __DELETING__ - Outbound connection deletion is in progress.
--
-- -   __DELETED__ - Outbound connection is deleted and can no longer be
--     used.
outboundConnectionStatus_statusCode :: Lens.Lens' OutboundConnectionStatus (Prelude.Maybe OutboundConnectionStatusCode)
outboundConnectionStatus_statusCode = Lens.lens (\OutboundConnectionStatus' {statusCode} -> statusCode) (\s@OutboundConnectionStatus' {} a -> s {statusCode = a} :: OutboundConnectionStatus)

instance Data.FromJSON OutboundConnectionStatus where
  parseJSON =
    Data.withObject
      "OutboundConnectionStatus"
      ( \x ->
          OutboundConnectionStatus'
            Prelude.<$> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "StatusCode")
      )

instance Prelude.Hashable OutboundConnectionStatus where
  hashWithSalt _salt OutboundConnectionStatus' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData OutboundConnectionStatus where
  rnf OutboundConnectionStatus' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf statusCode
