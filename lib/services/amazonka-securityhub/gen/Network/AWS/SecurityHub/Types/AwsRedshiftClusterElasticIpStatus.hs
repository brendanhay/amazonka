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
-- Module      : Network.AWS.SecurityHub.Types.AwsRedshiftClusterElasticIpStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsRedshiftClusterElasticIpStatus where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The status of the elastic IP (EIP) address for an Amazon Redshift
-- cluster.
--
-- /See:/ 'newAwsRedshiftClusterElasticIpStatus' smart constructor.
data AwsRedshiftClusterElasticIpStatus = AwsRedshiftClusterElasticIpStatus'
  { -- | The status of the elastic IP address.
    status :: Prelude.Maybe Prelude.Text,
    -- | The elastic IP address for the cluster.
    elasticIp :: Prelude.Maybe Prelude.Text
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
-- 'status', 'awsRedshiftClusterElasticIpStatus_status' - The status of the elastic IP address.
--
-- 'elasticIp', 'awsRedshiftClusterElasticIpStatus_elasticIp' - The elastic IP address for the cluster.
newAwsRedshiftClusterElasticIpStatus ::
  AwsRedshiftClusterElasticIpStatus
newAwsRedshiftClusterElasticIpStatus =
  AwsRedshiftClusterElasticIpStatus'
    { status =
        Prelude.Nothing,
      elasticIp = Prelude.Nothing
    }

-- | The status of the elastic IP address.
awsRedshiftClusterElasticIpStatus_status :: Lens.Lens' AwsRedshiftClusterElasticIpStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterElasticIpStatus_status = Lens.lens (\AwsRedshiftClusterElasticIpStatus' {status} -> status) (\s@AwsRedshiftClusterElasticIpStatus' {} a -> s {status = a} :: AwsRedshiftClusterElasticIpStatus)

-- | The elastic IP address for the cluster.
awsRedshiftClusterElasticIpStatus_elasticIp :: Lens.Lens' AwsRedshiftClusterElasticIpStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterElasticIpStatus_elasticIp = Lens.lens (\AwsRedshiftClusterElasticIpStatus' {elasticIp} -> elasticIp) (\s@AwsRedshiftClusterElasticIpStatus' {} a -> s {elasticIp = a} :: AwsRedshiftClusterElasticIpStatus)

instance
  Core.FromJSON
    AwsRedshiftClusterElasticIpStatus
  where
  parseJSON =
    Core.withObject
      "AwsRedshiftClusterElasticIpStatus"
      ( \x ->
          AwsRedshiftClusterElasticIpStatus'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ElasticIp")
      )

instance
  Prelude.Hashable
    AwsRedshiftClusterElasticIpStatus

instance
  Prelude.NFData
    AwsRedshiftClusterElasticIpStatus

instance
  Core.ToJSON
    AwsRedshiftClusterElasticIpStatus
  where
  toJSON AwsRedshiftClusterElasticIpStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Status" Core..=) Prelude.<$> status,
            ("ElasticIp" Core..=) Prelude.<$> elasticIp
          ]
      )
