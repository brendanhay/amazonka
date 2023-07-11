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
-- Module      : Amazonka.SecurityHub.Types.AwsRedshiftClusterElasticIpStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRedshiftClusterElasticIpStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The status of the elastic IP (EIP) address for an Amazon Redshift
-- cluster.
--
-- /See:/ 'newAwsRedshiftClusterElasticIpStatus' smart constructor.
data AwsRedshiftClusterElasticIpStatus = AwsRedshiftClusterElasticIpStatus'
  { -- | The elastic IP address for the cluster.
    elasticIp :: Prelude.Maybe Prelude.Text,
    -- | The status of the elastic IP address.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRedshiftClusterElasticIpStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'elasticIp', 'awsRedshiftClusterElasticIpStatus_elasticIp' - The elastic IP address for the cluster.
--
-- 'status', 'awsRedshiftClusterElasticIpStatus_status' - The status of the elastic IP address.
newAwsRedshiftClusterElasticIpStatus ::
  AwsRedshiftClusterElasticIpStatus
newAwsRedshiftClusterElasticIpStatus =
  AwsRedshiftClusterElasticIpStatus'
    { elasticIp =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The elastic IP address for the cluster.
awsRedshiftClusterElasticIpStatus_elasticIp :: Lens.Lens' AwsRedshiftClusterElasticIpStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterElasticIpStatus_elasticIp = Lens.lens (\AwsRedshiftClusterElasticIpStatus' {elasticIp} -> elasticIp) (\s@AwsRedshiftClusterElasticIpStatus' {} a -> s {elasticIp = a} :: AwsRedshiftClusterElasticIpStatus)

-- | The status of the elastic IP address.
awsRedshiftClusterElasticIpStatus_status :: Lens.Lens' AwsRedshiftClusterElasticIpStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterElasticIpStatus_status = Lens.lens (\AwsRedshiftClusterElasticIpStatus' {status} -> status) (\s@AwsRedshiftClusterElasticIpStatus' {} a -> s {status = a} :: AwsRedshiftClusterElasticIpStatus)

instance
  Data.FromJSON
    AwsRedshiftClusterElasticIpStatus
  where
  parseJSON =
    Data.withObject
      "AwsRedshiftClusterElasticIpStatus"
      ( \x ->
          AwsRedshiftClusterElasticIpStatus'
            Prelude.<$> (x Data..:? "ElasticIp")
            Prelude.<*> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    AwsRedshiftClusterElasticIpStatus
  where
  hashWithSalt
    _salt
    AwsRedshiftClusterElasticIpStatus' {..} =
      _salt
        `Prelude.hashWithSalt` elasticIp
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    AwsRedshiftClusterElasticIpStatus
  where
  rnf AwsRedshiftClusterElasticIpStatus' {..} =
    Prelude.rnf elasticIp
      `Prelude.seq` Prelude.rnf status

instance
  Data.ToJSON
    AwsRedshiftClusterElasticIpStatus
  where
  toJSON AwsRedshiftClusterElasticIpStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ElasticIp" Data..=) Prelude.<$> elasticIp,
            ("Status" Data..=) Prelude.<$> status
          ]
      )
