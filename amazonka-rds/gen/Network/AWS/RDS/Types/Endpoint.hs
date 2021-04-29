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
-- Module      : Network.AWS.RDS.Types.Endpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Endpoint where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    address :: Prelude.Maybe Prelude.Text,
    -- | Specifies the ID that Amazon Route 53 assigns when you create a hosted
    -- zone.
    hostedZoneId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the port that the database engine is listening on.
    port :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { address = Prelude.Nothing,
      hostedZoneId = Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | Specifies the DNS address of the DB instance.
endpoint_address :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_address = Lens.lens (\Endpoint' {address} -> address) (\s@Endpoint' {} a -> s {address = a} :: Endpoint)

-- | Specifies the ID that Amazon Route 53 assigns when you create a hosted
-- zone.
endpoint_hostedZoneId :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_hostedZoneId = Lens.lens (\Endpoint' {hostedZoneId} -> hostedZoneId) (\s@Endpoint' {} a -> s {hostedZoneId = a} :: Endpoint)

-- | Specifies the port that the database engine is listening on.
endpoint_port :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Int)
endpoint_port = Lens.lens (\Endpoint' {port} -> port) (\s@Endpoint' {} a -> s {port = a} :: Endpoint)

instance Prelude.FromXML Endpoint where
  parseXML x =
    Endpoint'
      Prelude.<$> (x Prelude..@? "Address")
      Prelude.<*> (x Prelude..@? "HostedZoneId")
      Prelude.<*> (x Prelude..@? "Port")

instance Prelude.Hashable Endpoint

instance Prelude.NFData Endpoint
