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
-- Module      : Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Configuration options for zone awareness.
--
-- /See:/ 'newAwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails' smart constructor.
data AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails = AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails'
  { -- | The number of Availability Zones that the domain uses. Valid values are
    -- @2@ or @3@. The default is @2@.
    availabilityZoneCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZoneCount', 'awsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails_availabilityZoneCount' - The number of Availability Zones that the domain uses. Valid values are
-- @2@ or @3@. The default is @2@.
newAwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails ::
  AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails
newAwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails =
  AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails'
    { availabilityZoneCount =
        Prelude.Nothing
    }

-- | The number of Availability Zones that the domain uses. Valid values are
-- @2@ or @3@. The default is @2@.
awsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails_availabilityZoneCount :: Lens.Lens' AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails (Prelude.Maybe Prelude.Int)
awsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails_availabilityZoneCount = Lens.lens (\AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails' {availabilityZoneCount} -> availabilityZoneCount) (\s@AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails' {} a -> s {availabilityZoneCount = a} :: AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails)

instance
  Core.FromJSON
    AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails
  where
  parseJSON =
    Core.withObject
      "AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails"
      ( \x ->
          AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails'
            Prelude.<$> (x Core..:? "AvailabilityZoneCount")
      )

instance
  Prelude.Hashable
    AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails
  where
  hashWithSalt
    _salt
    AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails' {..} =
      _salt `Prelude.hashWithSalt` availabilityZoneCount

instance
  Prelude.NFData
    AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails
  where
  rnf
    AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails' {..} =
      Prelude.rnf availabilityZoneCount

instance
  Core.ToJSON
    AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails
  where
  toJSON
    AwsOpenSearchServiceDomainClusterConfigZoneAwarenessConfigDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("AvailabilityZoneCount" Core..=)
                Prelude.<$> availabilityZoneCount
            ]
        )
