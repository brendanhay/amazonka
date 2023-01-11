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
-- Module      : Amazonka.NetworkFirewall.Types.Attachment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.Attachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.AttachmentStatus
import qualified Amazonka.Prelude as Prelude

-- | The configuration and status for a single subnet that you\'ve specified
-- for use by the Network Firewall firewall. This is part of the
-- FirewallStatus.
--
-- /See:/ 'newAttachment' smart constructor.
data Attachment = Attachment'
  { -- | The identifier of the firewall endpoint that Network Firewall has
    -- instantiated in the subnet. You use this to identify the firewall
    -- endpoint in the VPC route tables, when you redirect the VPC traffic
    -- through the endpoint.
    endpointId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the firewall endpoint in the subnet. This value
    -- reflects both the instantiation of the endpoint in the VPC subnet and
    -- the sync states that are reported in the @Config@ settings. When this
    -- value is @READY@, the endpoint is available and configured properly to
    -- handle network traffic. When the endpoint isn\'t available for traffic,
    -- this value will reflect its state, for example @CREATING@ or @DELETING@.
    status :: Prelude.Maybe AttachmentStatus,
    -- | If Network Firewall fails to create or delete the firewall endpoint in
    -- the subnet, it populates this with the reason for the failure and how to
    -- resolve it. Depending on the error, it can take as many as 15 minutes to
    -- populate this field. For more information about the errors and solutions
    -- available for this field, see
    -- <https://docs.aws.amazon.com/network-firewall/latest/developerguide/firewall-troubleshooting-endpoint-failures.html Troubleshooting firewall endpoint failures>
    -- in the /Network Firewall Developer Guide/.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the subnet that you\'ve specified to be used
    -- for a firewall endpoint.
    subnetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Attachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointId', 'attachment_endpointId' - The identifier of the firewall endpoint that Network Firewall has
-- instantiated in the subnet. You use this to identify the firewall
-- endpoint in the VPC route tables, when you redirect the VPC traffic
-- through the endpoint.
--
-- 'status', 'attachment_status' - The current status of the firewall endpoint in the subnet. This value
-- reflects both the instantiation of the endpoint in the VPC subnet and
-- the sync states that are reported in the @Config@ settings. When this
-- value is @READY@, the endpoint is available and configured properly to
-- handle network traffic. When the endpoint isn\'t available for traffic,
-- this value will reflect its state, for example @CREATING@ or @DELETING@.
--
-- 'statusMessage', 'attachment_statusMessage' - If Network Firewall fails to create or delete the firewall endpoint in
-- the subnet, it populates this with the reason for the failure and how to
-- resolve it. Depending on the error, it can take as many as 15 minutes to
-- populate this field. For more information about the errors and solutions
-- available for this field, see
-- <https://docs.aws.amazon.com/network-firewall/latest/developerguide/firewall-troubleshooting-endpoint-failures.html Troubleshooting firewall endpoint failures>
-- in the /Network Firewall Developer Guide/.
--
-- 'subnetId', 'attachment_subnetId' - The unique identifier of the subnet that you\'ve specified to be used
-- for a firewall endpoint.
newAttachment ::
  Attachment
newAttachment =
  Attachment'
    { endpointId = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      subnetId = Prelude.Nothing
    }

-- | The identifier of the firewall endpoint that Network Firewall has
-- instantiated in the subnet. You use this to identify the firewall
-- endpoint in the VPC route tables, when you redirect the VPC traffic
-- through the endpoint.
attachment_endpointId :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_endpointId = Lens.lens (\Attachment' {endpointId} -> endpointId) (\s@Attachment' {} a -> s {endpointId = a} :: Attachment)

-- | The current status of the firewall endpoint in the subnet. This value
-- reflects both the instantiation of the endpoint in the VPC subnet and
-- the sync states that are reported in the @Config@ settings. When this
-- value is @READY@, the endpoint is available and configured properly to
-- handle network traffic. When the endpoint isn\'t available for traffic,
-- this value will reflect its state, for example @CREATING@ or @DELETING@.
attachment_status :: Lens.Lens' Attachment (Prelude.Maybe AttachmentStatus)
attachment_status = Lens.lens (\Attachment' {status} -> status) (\s@Attachment' {} a -> s {status = a} :: Attachment)

-- | If Network Firewall fails to create or delete the firewall endpoint in
-- the subnet, it populates this with the reason for the failure and how to
-- resolve it. Depending on the error, it can take as many as 15 minutes to
-- populate this field. For more information about the errors and solutions
-- available for this field, see
-- <https://docs.aws.amazon.com/network-firewall/latest/developerguide/firewall-troubleshooting-endpoint-failures.html Troubleshooting firewall endpoint failures>
-- in the /Network Firewall Developer Guide/.
attachment_statusMessage :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_statusMessage = Lens.lens (\Attachment' {statusMessage} -> statusMessage) (\s@Attachment' {} a -> s {statusMessage = a} :: Attachment)

-- | The unique identifier of the subnet that you\'ve specified to be used
-- for a firewall endpoint.
attachment_subnetId :: Lens.Lens' Attachment (Prelude.Maybe Prelude.Text)
attachment_subnetId = Lens.lens (\Attachment' {subnetId} -> subnetId) (\s@Attachment' {} a -> s {subnetId = a} :: Attachment)

instance Data.FromJSON Attachment where
  parseJSON =
    Data.withObject
      "Attachment"
      ( \x ->
          Attachment'
            Prelude.<$> (x Data..:? "EndpointId")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusMessage")
            Prelude.<*> (x Data..:? "SubnetId")
      )

instance Prelude.Hashable Attachment where
  hashWithSalt _salt Attachment' {..} =
    _salt `Prelude.hashWithSalt` endpointId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` subnetId

instance Prelude.NFData Attachment where
  rnf Attachment' {..} =
    Prelude.rnf endpointId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf subnetId
