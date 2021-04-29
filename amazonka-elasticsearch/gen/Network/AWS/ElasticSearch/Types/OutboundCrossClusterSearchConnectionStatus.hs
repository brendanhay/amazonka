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
-- Module      : Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatus where

import Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatusCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the connection status of an outbound cross-cluster search
-- connection.
--
-- /See:/ 'newOutboundCrossClusterSearchConnectionStatus' smart constructor.
data OutboundCrossClusterSearchConnectionStatus = OutboundCrossClusterSearchConnectionStatus'
  { -- | Specifies verbose information for the outbound connection status.
    message :: Prelude.Maybe Prelude.Text,
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
    statusCode :: Prelude.Maybe OutboundCrossClusterSearchConnectionStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      statusCode = Prelude.Nothing
    }

-- | Specifies verbose information for the outbound connection status.
outboundCrossClusterSearchConnectionStatus_message :: Lens.Lens' OutboundCrossClusterSearchConnectionStatus (Prelude.Maybe Prelude.Text)
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
outboundCrossClusterSearchConnectionStatus_statusCode :: Lens.Lens' OutboundCrossClusterSearchConnectionStatus (Prelude.Maybe OutboundCrossClusterSearchConnectionStatusCode)
outboundCrossClusterSearchConnectionStatus_statusCode = Lens.lens (\OutboundCrossClusterSearchConnectionStatus' {statusCode} -> statusCode) (\s@OutboundCrossClusterSearchConnectionStatus' {} a -> s {statusCode = a} :: OutboundCrossClusterSearchConnectionStatus)

instance
  Prelude.FromJSON
    OutboundCrossClusterSearchConnectionStatus
  where
  parseJSON =
    Prelude.withObject
      "OutboundCrossClusterSearchConnectionStatus"
      ( \x ->
          OutboundCrossClusterSearchConnectionStatus'
            Prelude.<$> (x Prelude..:? "Message")
              Prelude.<*> (x Prelude..:? "StatusCode")
      )

instance
  Prelude.Hashable
    OutboundCrossClusterSearchConnectionStatus

instance
  Prelude.NFData
    OutboundCrossClusterSearchConnectionStatus
