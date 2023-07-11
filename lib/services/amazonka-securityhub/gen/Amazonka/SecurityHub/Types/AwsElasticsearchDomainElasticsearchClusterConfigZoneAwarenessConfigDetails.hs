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
-- Module      : Amazonka.SecurityHub.Types.AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration options for zone awareness.
--
-- /See:/ 'newAwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails' smart constructor.
data AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails = AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails'
  { -- | he number of Availability Zones that the domain uses. Valid values are 2
    -- and 3. The default is 2.
    availabilityZoneCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZoneCount', 'awsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails_availabilityZoneCount' - he number of Availability Zones that the domain uses. Valid values are 2
-- and 3. The default is 2.
newAwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails ::
  AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails
newAwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails =
  AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails'
    { availabilityZoneCount =
        Prelude.Nothing
    }

-- | he number of Availability Zones that the domain uses. Valid values are 2
-- and 3. The default is 2.
awsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails_availabilityZoneCount :: Lens.Lens' AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails (Prelude.Maybe Prelude.Int)
awsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails_availabilityZoneCount = Lens.lens (\AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails' {availabilityZoneCount} -> availabilityZoneCount) (\s@AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails' {} a -> s {availabilityZoneCount = a} :: AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails)

instance
  Data.FromJSON
    AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails
  where
  parseJSON =
    Data.withObject
      "AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails"
      ( \x ->
          AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails'
            Prelude.<$> (x Data..:? "AvailabilityZoneCount")
      )

instance
  Prelude.Hashable
    AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails
  where
  hashWithSalt
    _salt
    AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails' {..} =
      _salt `Prelude.hashWithSalt` availabilityZoneCount

instance
  Prelude.NFData
    AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails
  where
  rnf
    AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails' {..} =
      Prelude.rnf availabilityZoneCount

instance
  Data.ToJSON
    AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails
  where
  toJSON
    AwsElasticsearchDomainElasticsearchClusterConfigZoneAwarenessConfigDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("AvailabilityZoneCount" Data..=)
                Prelude.<$> availabilityZoneCount
            ]
        )
