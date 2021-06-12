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
-- Module      : Network.AWS.RDS.Types.Endpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Endpoint where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | This data type represents the information you need to connect to an
-- Amazon RDS DB instance. This data type is used as a response element in
-- the following actions:
--
-- -   @CreateDBInstance@
--
-- -   @DescribeDBInstances@
--
-- -   @DeleteDBInstance@
--
-- For the data structure that represents Amazon Aurora DB cluster
-- endpoints, see @DBClusterEndpoint@.
--
-- /See:/ 'newEndpoint' smart constructor.
data Endpoint = Endpoint'
  { -- | Specifies the DNS address of the DB instance.
    address :: Core.Maybe Core.Text,
    -- | Specifies the ID that Amazon Route 53 assigns when you create a hosted
    -- zone.
    hostedZoneId :: Core.Maybe Core.Text,
    -- | Specifies the port that the database engine is listening on.
    port :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Endpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'endpoint_address' - Specifies the DNS address of the DB instance.
--
-- 'hostedZoneId', 'endpoint_hostedZoneId' - Specifies the ID that Amazon Route 53 assigns when you create a hosted
-- zone.
--
-- 'port', 'endpoint_port' - Specifies the port that the database engine is listening on.
newEndpoint ::
  Endpoint
newEndpoint =
  Endpoint'
    { address = Core.Nothing,
      hostedZoneId = Core.Nothing,
      port = Core.Nothing
    }

-- | Specifies the DNS address of the DB instance.
endpoint_address :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
endpoint_address = Lens.lens (\Endpoint' {address} -> address) (\s@Endpoint' {} a -> s {address = a} :: Endpoint)

-- | Specifies the ID that Amazon Route 53 assigns when you create a hosted
-- zone.
endpoint_hostedZoneId :: Lens.Lens' Endpoint (Core.Maybe Core.Text)
endpoint_hostedZoneId = Lens.lens (\Endpoint' {hostedZoneId} -> hostedZoneId) (\s@Endpoint' {} a -> s {hostedZoneId = a} :: Endpoint)

-- | Specifies the port that the database engine is listening on.
endpoint_port :: Lens.Lens' Endpoint (Core.Maybe Core.Int)
endpoint_port = Lens.lens (\Endpoint' {port} -> port) (\s@Endpoint' {} a -> s {port = a} :: Endpoint)

instance Core.FromXML Endpoint where
  parseXML x =
    Endpoint'
      Core.<$> (x Core..@? "Address")
      Core.<*> (x Core..@? "HostedZoneId")
      Core.<*> (x Core..@? "Port")

instance Core.Hashable Endpoint

instance Core.NFData Endpoint
