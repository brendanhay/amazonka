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
-- Module      : Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatus where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatusCode
import qualified Network.AWS.Lens as Lens

-- | Specifies the connection status of an outbound cross-cluster search
-- connection.
--
-- /See:/ 'newOutboundCrossClusterSearchConnectionStatus' smart constructor.
data OutboundCrossClusterSearchConnectionStatus = OutboundCrossClusterSearchConnectionStatus'
  { -- | Specifies verbose information for the outbound connection status.
    message :: Core.Maybe Core.Text,
    -- | The state code for outbound connection. This can be one of the
    -- following:
    --
    -- -   VALIDATING: The outbound connection request is being validated.
    -- -   VALIDATION_FAILED: Validation failed for the connection request.
    -- -   PENDING_ACCEPTANCE: Outbound connection request is validated and is
    --     not yet accepted by destination domain owner.
    -- -   PROVISIONING: Outbound connection request is in process.
    -- -   ACTIVE: Outbound connection is active and ready to use.
    -- -   REJECTED: Outbound connection request is rejected by destination
    --     domain owner.
    -- -   DELETING: Outbound connection deletion is in progress.
    -- -   DELETED: Outbound connection is deleted and cannot be used further.
    statusCode :: Core.Maybe OutboundCrossClusterSearchConnectionStatusCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OutboundCrossClusterSearchConnectionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'outboundCrossClusterSearchConnectionStatus_message' - Specifies verbose information for the outbound connection status.
--
-- 'statusCode', 'outboundCrossClusterSearchConnectionStatus_statusCode' - The state code for outbound connection. This can be one of the
-- following:
--
-- -   VALIDATING: The outbound connection request is being validated.
-- -   VALIDATION_FAILED: Validation failed for the connection request.
-- -   PENDING_ACCEPTANCE: Outbound connection request is validated and is
--     not yet accepted by destination domain owner.
-- -   PROVISIONING: Outbound connection request is in process.
-- -   ACTIVE: Outbound connection is active and ready to use.
-- -   REJECTED: Outbound connection request is rejected by destination
--     domain owner.
-- -   DELETING: Outbound connection deletion is in progress.
-- -   DELETED: Outbound connection is deleted and cannot be used further.
newOutboundCrossClusterSearchConnectionStatus ::
  OutboundCrossClusterSearchConnectionStatus
newOutboundCrossClusterSearchConnectionStatus =
  OutboundCrossClusterSearchConnectionStatus'
    { message =
        Core.Nothing,
      statusCode = Core.Nothing
    }

-- | Specifies verbose information for the outbound connection status.
outboundCrossClusterSearchConnectionStatus_message :: Lens.Lens' OutboundCrossClusterSearchConnectionStatus (Core.Maybe Core.Text)
outboundCrossClusterSearchConnectionStatus_message = Lens.lens (\OutboundCrossClusterSearchConnectionStatus' {message} -> message) (\s@OutboundCrossClusterSearchConnectionStatus' {} a -> s {message = a} :: OutboundCrossClusterSearchConnectionStatus)

-- | The state code for outbound connection. This can be one of the
-- following:
--
-- -   VALIDATING: The outbound connection request is being validated.
-- -   VALIDATION_FAILED: Validation failed for the connection request.
-- -   PENDING_ACCEPTANCE: Outbound connection request is validated and is
--     not yet accepted by destination domain owner.
-- -   PROVISIONING: Outbound connection request is in process.
-- -   ACTIVE: Outbound connection is active and ready to use.
-- -   REJECTED: Outbound connection request is rejected by destination
--     domain owner.
-- -   DELETING: Outbound connection deletion is in progress.
-- -   DELETED: Outbound connection is deleted and cannot be used further.
outboundCrossClusterSearchConnectionStatus_statusCode :: Lens.Lens' OutboundCrossClusterSearchConnectionStatus (Core.Maybe OutboundCrossClusterSearchConnectionStatusCode)
outboundCrossClusterSearchConnectionStatus_statusCode = Lens.lens (\OutboundCrossClusterSearchConnectionStatus' {statusCode} -> statusCode) (\s@OutboundCrossClusterSearchConnectionStatus' {} a -> s {statusCode = a} :: OutboundCrossClusterSearchConnectionStatus)

instance
  Core.FromJSON
    OutboundCrossClusterSearchConnectionStatus
  where
  parseJSON =
    Core.withObject
      "OutboundCrossClusterSearchConnectionStatus"
      ( \x ->
          OutboundCrossClusterSearchConnectionStatus'
            Core.<$> (x Core..:? "Message")
            Core.<*> (x Core..:? "StatusCode")
      )

instance
  Core.Hashable
    OutboundCrossClusterSearchConnectionStatus

instance
  Core.NFData
    OutboundCrossClusterSearchConnectionStatus
