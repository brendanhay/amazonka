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
-- Module      : Amazonka.SecurityHub.Types.AwsRdsDbInstanceEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbInstanceEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the connection endpoint.
--
-- /See:/ 'newAwsRdsDbInstanceEndpoint' smart constructor.
data AwsRdsDbInstanceEndpoint = AwsRdsDbInstanceEndpoint'
  { -- | Specifies the port that the database engine is listening on.
    port :: Prelude.Maybe Prelude.Int,
    -- | Specifies the ID that Amazon Route 53 assigns when you create a hosted
    -- zone.
    hostedZoneId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the DNS address of the DB instance.
    address :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsDbInstanceEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'awsRdsDbInstanceEndpoint_port' - Specifies the port that the database engine is listening on.
--
-- 'hostedZoneId', 'awsRdsDbInstanceEndpoint_hostedZoneId' - Specifies the ID that Amazon Route 53 assigns when you create a hosted
-- zone.
--
-- 'address', 'awsRdsDbInstanceEndpoint_address' - Specifies the DNS address of the DB instance.
newAwsRdsDbInstanceEndpoint ::
  AwsRdsDbInstanceEndpoint
newAwsRdsDbInstanceEndpoint =
  AwsRdsDbInstanceEndpoint'
    { port = Prelude.Nothing,
      hostedZoneId = Prelude.Nothing,
      address = Prelude.Nothing
    }

-- | Specifies the port that the database engine is listening on.
awsRdsDbInstanceEndpoint_port :: Lens.Lens' AwsRdsDbInstanceEndpoint (Prelude.Maybe Prelude.Int)
awsRdsDbInstanceEndpoint_port = Lens.lens (\AwsRdsDbInstanceEndpoint' {port} -> port) (\s@AwsRdsDbInstanceEndpoint' {} a -> s {port = a} :: AwsRdsDbInstanceEndpoint)

-- | Specifies the ID that Amazon Route 53 assigns when you create a hosted
-- zone.
awsRdsDbInstanceEndpoint_hostedZoneId :: Lens.Lens' AwsRdsDbInstanceEndpoint (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceEndpoint_hostedZoneId = Lens.lens (\AwsRdsDbInstanceEndpoint' {hostedZoneId} -> hostedZoneId) (\s@AwsRdsDbInstanceEndpoint' {} a -> s {hostedZoneId = a} :: AwsRdsDbInstanceEndpoint)

-- | Specifies the DNS address of the DB instance.
awsRdsDbInstanceEndpoint_address :: Lens.Lens' AwsRdsDbInstanceEndpoint (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceEndpoint_address = Lens.lens (\AwsRdsDbInstanceEndpoint' {address} -> address) (\s@AwsRdsDbInstanceEndpoint' {} a -> s {address = a} :: AwsRdsDbInstanceEndpoint)

instance Data.FromJSON AwsRdsDbInstanceEndpoint where
  parseJSON =
    Data.withObject
      "AwsRdsDbInstanceEndpoint"
      ( \x ->
          AwsRdsDbInstanceEndpoint'
            Prelude.<$> (x Data..:? "Port")
            Prelude.<*> (x Data..:? "HostedZoneId")
            Prelude.<*> (x Data..:? "Address")
      )

instance Prelude.Hashable AwsRdsDbInstanceEndpoint where
  hashWithSalt _salt AwsRdsDbInstanceEndpoint' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` hostedZoneId
      `Prelude.hashWithSalt` address

instance Prelude.NFData AwsRdsDbInstanceEndpoint where
  rnf AwsRdsDbInstanceEndpoint' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf hostedZoneId
      `Prelude.seq` Prelude.rnf address

instance Data.ToJSON AwsRdsDbInstanceEndpoint where
  toJSON AwsRdsDbInstanceEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Port" Data..=) Prelude.<$> port,
            ("HostedZoneId" Data..=) Prelude.<$> hostedZoneId,
            ("Address" Data..=) Prelude.<$> address
          ]
      )
