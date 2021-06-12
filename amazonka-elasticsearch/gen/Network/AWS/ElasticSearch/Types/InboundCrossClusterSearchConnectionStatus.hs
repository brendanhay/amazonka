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
-- Module      : Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatus where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatusCode
import qualified Network.AWS.Lens as Lens

-- | Specifies the coonection status of an inbound cross-cluster search
-- connection.
--
-- /See:/ 'newInboundCrossClusterSearchConnectionStatus' smart constructor.
data InboundCrossClusterSearchConnectionStatus = InboundCrossClusterSearchConnectionStatus'
  { -- | Specifies verbose information for the inbound connection status.
    message :: Core.Maybe Core.Text,
    -- | The state code for inbound connection. This can be one of the following:
    --
    -- -   PENDING_ACCEPTANCE: Inbound connection is not yet accepted by
    --     destination domain owner.
    -- -   APPROVED: Inbound connection is pending acceptance by destination
    --     domain owner.
    -- -   REJECTING: Inbound connection rejection is in process.
    -- -   REJECTED: Inbound connection is rejected.
    -- -   DELETING: Inbound connection deletion is in progress.
    -- -   DELETED: Inbound connection is deleted and cannot be used further.
    statusCode :: Core.Maybe InboundCrossClusterSearchConnectionStatusCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InboundCrossClusterSearchConnectionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'inboundCrossClusterSearchConnectionStatus_message' - Specifies verbose information for the inbound connection status.
--
-- 'statusCode', 'inboundCrossClusterSearchConnectionStatus_statusCode' - The state code for inbound connection. This can be one of the following:
--
-- -   PENDING_ACCEPTANCE: Inbound connection is not yet accepted by
--     destination domain owner.
-- -   APPROVED: Inbound connection is pending acceptance by destination
--     domain owner.
-- -   REJECTING: Inbound connection rejection is in process.
-- -   REJECTED: Inbound connection is rejected.
-- -   DELETING: Inbound connection deletion is in progress.
-- -   DELETED: Inbound connection is deleted and cannot be used further.
newInboundCrossClusterSearchConnectionStatus ::
  InboundCrossClusterSearchConnectionStatus
newInboundCrossClusterSearchConnectionStatus =
  InboundCrossClusterSearchConnectionStatus'
    { message =
        Core.Nothing,
      statusCode = Core.Nothing
    }

-- | Specifies verbose information for the inbound connection status.
inboundCrossClusterSearchConnectionStatus_message :: Lens.Lens' InboundCrossClusterSearchConnectionStatus (Core.Maybe Core.Text)
inboundCrossClusterSearchConnectionStatus_message = Lens.lens (\InboundCrossClusterSearchConnectionStatus' {message} -> message) (\s@InboundCrossClusterSearchConnectionStatus' {} a -> s {message = a} :: InboundCrossClusterSearchConnectionStatus)

-- | The state code for inbound connection. This can be one of the following:
--
-- -   PENDING_ACCEPTANCE: Inbound connection is not yet accepted by
--     destination domain owner.
-- -   APPROVED: Inbound connection is pending acceptance by destination
--     domain owner.
-- -   REJECTING: Inbound connection rejection is in process.
-- -   REJECTED: Inbound connection is rejected.
-- -   DELETING: Inbound connection deletion is in progress.
-- -   DELETED: Inbound connection is deleted and cannot be used further.
inboundCrossClusterSearchConnectionStatus_statusCode :: Lens.Lens' InboundCrossClusterSearchConnectionStatus (Core.Maybe InboundCrossClusterSearchConnectionStatusCode)
inboundCrossClusterSearchConnectionStatus_statusCode = Lens.lens (\InboundCrossClusterSearchConnectionStatus' {statusCode} -> statusCode) (\s@InboundCrossClusterSearchConnectionStatus' {} a -> s {statusCode = a} :: InboundCrossClusterSearchConnectionStatus)

instance
  Core.FromJSON
    InboundCrossClusterSearchConnectionStatus
  where
  parseJSON =
    Core.withObject
      "InboundCrossClusterSearchConnectionStatus"
      ( \x ->
          InboundCrossClusterSearchConnectionStatus'
            Core.<$> (x Core..:? "Message")
            Core.<*> (x Core..:? "StatusCode")
      )

instance
  Core.Hashable
    InboundCrossClusterSearchConnectionStatus

instance
  Core.NFData
    InboundCrossClusterSearchConnectionStatus
