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
-- Module      : Amazonka.SecurityHub.Types.AwsRedshiftClusterEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRedshiftClusterEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The connection endpoint for an Amazon Redshift cluster.
--
-- /See:/ 'newAwsRedshiftClusterEndpoint' smart constructor.
data AwsRedshiftClusterEndpoint = AwsRedshiftClusterEndpoint'
  { -- | The port that the database engine listens on.
    port :: Prelude.Maybe Prelude.Int,
    -- | The DNS address of the cluster.
    address :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRedshiftClusterEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'awsRedshiftClusterEndpoint_port' - The port that the database engine listens on.
--
-- 'address', 'awsRedshiftClusterEndpoint_address' - The DNS address of the cluster.
newAwsRedshiftClusterEndpoint ::
  AwsRedshiftClusterEndpoint
newAwsRedshiftClusterEndpoint =
  AwsRedshiftClusterEndpoint'
    { port = Prelude.Nothing,
      address = Prelude.Nothing
    }

-- | The port that the database engine listens on.
awsRedshiftClusterEndpoint_port :: Lens.Lens' AwsRedshiftClusterEndpoint (Prelude.Maybe Prelude.Int)
awsRedshiftClusterEndpoint_port = Lens.lens (\AwsRedshiftClusterEndpoint' {port} -> port) (\s@AwsRedshiftClusterEndpoint' {} a -> s {port = a} :: AwsRedshiftClusterEndpoint)

-- | The DNS address of the cluster.
awsRedshiftClusterEndpoint_address :: Lens.Lens' AwsRedshiftClusterEndpoint (Prelude.Maybe Prelude.Text)
awsRedshiftClusterEndpoint_address = Lens.lens (\AwsRedshiftClusterEndpoint' {address} -> address) (\s@AwsRedshiftClusterEndpoint' {} a -> s {address = a} :: AwsRedshiftClusterEndpoint)

instance Core.FromJSON AwsRedshiftClusterEndpoint where
  parseJSON =
    Core.withObject
      "AwsRedshiftClusterEndpoint"
      ( \x ->
          AwsRedshiftClusterEndpoint'
            Prelude.<$> (x Core..:? "Port")
            Prelude.<*> (x Core..:? "Address")
      )

instance Prelude.Hashable AwsRedshiftClusterEndpoint where
  hashWithSalt _salt AwsRedshiftClusterEndpoint' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` address

instance Prelude.NFData AwsRedshiftClusterEndpoint where
  rnf AwsRedshiftClusterEndpoint' {..} =
    Prelude.rnf port `Prelude.seq` Prelude.rnf address

instance Core.ToJSON AwsRedshiftClusterEndpoint where
  toJSON AwsRedshiftClusterEndpoint' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Port" Core..=) Prelude.<$> port,
            ("Address" Core..=) Prelude.<$> address
          ]
      )
