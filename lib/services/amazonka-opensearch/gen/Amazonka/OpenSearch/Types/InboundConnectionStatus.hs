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
-- Module      : Amazonka.OpenSearch.Types.InboundConnectionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.InboundConnectionStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.InboundConnectionStatusCode
import qualified Amazonka.Prelude as Prelude

-- | The status of an inbound cross-cluster connection for OpenSearch
-- Service.
--
-- /See:/ 'newInboundConnectionStatus' smart constructor.
data InboundConnectionStatus = InboundConnectionStatus'
  { -- | Information about the connection.
    message :: Prelude.Maybe Prelude.Text,
    -- | The status code for the connection. Can be one of the following:
    --
    -- -   __PENDING_ACCEPTANCE__ - Inbound connection is not yet accepted by
    --     the remote domain owner.
    --
    -- -   __APPROVED__: Inbound connection is pending acceptance by the remote
    --     domain owner.
    --
    -- -   __PROVISIONING__: Inbound connection is being provisioned.
    --
    -- -   __ACTIVE__: Inbound connection is active and ready to use.
    --
    -- -   __REJECTING__: Inbound connection rejection is in process.
    --
    -- -   __REJECTED__: Inbound connection is rejected.
    --
    -- -   __DELETING__: Inbound connection deletion is in progress.
    --
    -- -   __DELETED__: Inbound connection is deleted and can no longer be
    --     used.
    statusCode :: Prelude.Maybe InboundConnectionStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InboundConnectionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'inboundConnectionStatus_message' - Information about the connection.
--
-- 'statusCode', 'inboundConnectionStatus_statusCode' - The status code for the connection. Can be one of the following:
--
-- -   __PENDING_ACCEPTANCE__ - Inbound connection is not yet accepted by
--     the remote domain owner.
--
-- -   __APPROVED__: Inbound connection is pending acceptance by the remote
--     domain owner.
--
-- -   __PROVISIONING__: Inbound connection is being provisioned.
--
-- -   __ACTIVE__: Inbound connection is active and ready to use.
--
-- -   __REJECTING__: Inbound connection rejection is in process.
--
-- -   __REJECTED__: Inbound connection is rejected.
--
-- -   __DELETING__: Inbound connection deletion is in progress.
--
-- -   __DELETED__: Inbound connection is deleted and can no longer be
--     used.
newInboundConnectionStatus ::
  InboundConnectionStatus
newInboundConnectionStatus =
  InboundConnectionStatus'
    { message = Prelude.Nothing,
      statusCode = Prelude.Nothing
    }

-- | Information about the connection.
inboundConnectionStatus_message :: Lens.Lens' InboundConnectionStatus (Prelude.Maybe Prelude.Text)
inboundConnectionStatus_message = Lens.lens (\InboundConnectionStatus' {message} -> message) (\s@InboundConnectionStatus' {} a -> s {message = a} :: InboundConnectionStatus)

-- | The status code for the connection. Can be one of the following:
--
-- -   __PENDING_ACCEPTANCE__ - Inbound connection is not yet accepted by
--     the remote domain owner.
--
-- -   __APPROVED__: Inbound connection is pending acceptance by the remote
--     domain owner.
--
-- -   __PROVISIONING__: Inbound connection is being provisioned.
--
-- -   __ACTIVE__: Inbound connection is active and ready to use.
--
-- -   __REJECTING__: Inbound connection rejection is in process.
--
-- -   __REJECTED__: Inbound connection is rejected.
--
-- -   __DELETING__: Inbound connection deletion is in progress.
--
-- -   __DELETED__: Inbound connection is deleted and can no longer be
--     used.
inboundConnectionStatus_statusCode :: Lens.Lens' InboundConnectionStatus (Prelude.Maybe InboundConnectionStatusCode)
inboundConnectionStatus_statusCode = Lens.lens (\InboundConnectionStatus' {statusCode} -> statusCode) (\s@InboundConnectionStatus' {} a -> s {statusCode = a} :: InboundConnectionStatus)

instance Data.FromJSON InboundConnectionStatus where
  parseJSON =
    Data.withObject
      "InboundConnectionStatus"
      ( \x ->
          InboundConnectionStatus'
            Prelude.<$> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "StatusCode")
      )

instance Prelude.Hashable InboundConnectionStatus where
  hashWithSalt _salt InboundConnectionStatus' {..} =
    _salt
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData InboundConnectionStatus where
  rnf InboundConnectionStatus' {..} =
    Prelude.rnf message `Prelude.seq`
      Prelude.rnf statusCode
