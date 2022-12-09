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
-- Module      : Amazonka.Route53Resolver.Types.IpAddressResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.IpAddressResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53Resolver.Types.IpAddressStatus

-- | In the response to a
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_GetResolverEndpoint.html GetResolverEndpoint>
-- request, information about the IP addresses that the Resolver endpoint
-- uses for DNS queries.
--
-- /See:/ 'newIpAddressResponse' smart constructor.
data IpAddressResponse = IpAddressResponse'
  { -- | The date and time that the IP address was created, in Unix time format
    -- and Coordinated Universal Time (UTC).
    creationTime :: Prelude.Maybe Prelude.Text,
    -- | One IP address that the Resolver endpoint uses for DNS queries.
    ip :: Prelude.Maybe Prelude.Text,
    -- | The ID of one IP address.
    ipId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the IP address was last modified, in Unix time
    -- format and Coordinated Universal Time (UTC).
    modificationTime :: Prelude.Maybe Prelude.Text,
    -- | A status code that gives the current status of the request.
    status :: Prelude.Maybe IpAddressStatus,
    -- | A message that provides additional information about the status of the
    -- request.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of one subnet.
    subnetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpAddressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'ipAddressResponse_creationTime' - The date and time that the IP address was created, in Unix time format
-- and Coordinated Universal Time (UTC).
--
-- 'ip', 'ipAddressResponse_ip' - One IP address that the Resolver endpoint uses for DNS queries.
--
-- 'ipId', 'ipAddressResponse_ipId' - The ID of one IP address.
--
-- 'modificationTime', 'ipAddressResponse_modificationTime' - The date and time that the IP address was last modified, in Unix time
-- format and Coordinated Universal Time (UTC).
--
-- 'status', 'ipAddressResponse_status' - A status code that gives the current status of the request.
--
-- 'statusMessage', 'ipAddressResponse_statusMessage' - A message that provides additional information about the status of the
-- request.
--
-- 'subnetId', 'ipAddressResponse_subnetId' - The ID of one subnet.
newIpAddressResponse ::
  IpAddressResponse
newIpAddressResponse =
  IpAddressResponse'
    { creationTime = Prelude.Nothing,
      ip = Prelude.Nothing,
      ipId = Prelude.Nothing,
      modificationTime = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      subnetId = Prelude.Nothing
    }

-- | The date and time that the IP address was created, in Unix time format
-- and Coordinated Universal Time (UTC).
ipAddressResponse_creationTime :: Lens.Lens' IpAddressResponse (Prelude.Maybe Prelude.Text)
ipAddressResponse_creationTime = Lens.lens (\IpAddressResponse' {creationTime} -> creationTime) (\s@IpAddressResponse' {} a -> s {creationTime = a} :: IpAddressResponse)

-- | One IP address that the Resolver endpoint uses for DNS queries.
ipAddressResponse_ip :: Lens.Lens' IpAddressResponse (Prelude.Maybe Prelude.Text)
ipAddressResponse_ip = Lens.lens (\IpAddressResponse' {ip} -> ip) (\s@IpAddressResponse' {} a -> s {ip = a} :: IpAddressResponse)

-- | The ID of one IP address.
ipAddressResponse_ipId :: Lens.Lens' IpAddressResponse (Prelude.Maybe Prelude.Text)
ipAddressResponse_ipId = Lens.lens (\IpAddressResponse' {ipId} -> ipId) (\s@IpAddressResponse' {} a -> s {ipId = a} :: IpAddressResponse)

-- | The date and time that the IP address was last modified, in Unix time
-- format and Coordinated Universal Time (UTC).
ipAddressResponse_modificationTime :: Lens.Lens' IpAddressResponse (Prelude.Maybe Prelude.Text)
ipAddressResponse_modificationTime = Lens.lens (\IpAddressResponse' {modificationTime} -> modificationTime) (\s@IpAddressResponse' {} a -> s {modificationTime = a} :: IpAddressResponse)

-- | A status code that gives the current status of the request.
ipAddressResponse_status :: Lens.Lens' IpAddressResponse (Prelude.Maybe IpAddressStatus)
ipAddressResponse_status = Lens.lens (\IpAddressResponse' {status} -> status) (\s@IpAddressResponse' {} a -> s {status = a} :: IpAddressResponse)

-- | A message that provides additional information about the status of the
-- request.
ipAddressResponse_statusMessage :: Lens.Lens' IpAddressResponse (Prelude.Maybe Prelude.Text)
ipAddressResponse_statusMessage = Lens.lens (\IpAddressResponse' {statusMessage} -> statusMessage) (\s@IpAddressResponse' {} a -> s {statusMessage = a} :: IpAddressResponse)

-- | The ID of one subnet.
ipAddressResponse_subnetId :: Lens.Lens' IpAddressResponse (Prelude.Maybe Prelude.Text)
ipAddressResponse_subnetId = Lens.lens (\IpAddressResponse' {subnetId} -> subnetId) (\s@IpAddressResponse' {} a -> s {subnetId = a} :: IpAddressResponse)

instance Data.FromJSON IpAddressResponse where
  parseJSON =
    Data.withObject
      "IpAddressResponse"
      ( \x ->
          IpAddressResponse'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "Ip")
            Prelude.<*> (x Data..:? "IpId")
            Prelude.<*> (x Data..:? "ModificationTime")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusMessage")
            Prelude.<*> (x Data..:? "SubnetId")
      )

instance Prelude.Hashable IpAddressResponse where
  hashWithSalt _salt IpAddressResponse' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` ip
      `Prelude.hashWithSalt` ipId
      `Prelude.hashWithSalt` modificationTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` subnetId

instance Prelude.NFData IpAddressResponse where
  rnf IpAddressResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf ip
      `Prelude.seq` Prelude.rnf ipId
      `Prelude.seq` Prelude.rnf modificationTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf subnetId
