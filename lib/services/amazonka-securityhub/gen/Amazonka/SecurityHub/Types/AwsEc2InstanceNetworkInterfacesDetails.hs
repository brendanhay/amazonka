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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2InstanceNetworkInterfacesDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2InstanceNetworkInterfacesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Identifies a network interface for the Amazon EC2 instance.
--
-- /See:/ 'newAwsEc2InstanceNetworkInterfacesDetails' smart constructor.
data AwsEc2InstanceNetworkInterfacesDetails = AwsEc2InstanceNetworkInterfacesDetails'
  { -- | The identifier of the network interface. The details are in a
    -- corresponding @AwsEc2NetworkInterfacesDetails@ object.
    networkInterfaceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2InstanceNetworkInterfacesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkInterfaceId', 'awsEc2InstanceNetworkInterfacesDetails_networkInterfaceId' - The identifier of the network interface. The details are in a
-- corresponding @AwsEc2NetworkInterfacesDetails@ object.
newAwsEc2InstanceNetworkInterfacesDetails ::
  AwsEc2InstanceNetworkInterfacesDetails
newAwsEc2InstanceNetworkInterfacesDetails =
  AwsEc2InstanceNetworkInterfacesDetails'
    { networkInterfaceId =
        Prelude.Nothing
    }

-- | The identifier of the network interface. The details are in a
-- corresponding @AwsEc2NetworkInterfacesDetails@ object.
awsEc2InstanceNetworkInterfacesDetails_networkInterfaceId :: Lens.Lens' AwsEc2InstanceNetworkInterfacesDetails (Prelude.Maybe Prelude.Text)
awsEc2InstanceNetworkInterfacesDetails_networkInterfaceId = Lens.lens (\AwsEc2InstanceNetworkInterfacesDetails' {networkInterfaceId} -> networkInterfaceId) (\s@AwsEc2InstanceNetworkInterfacesDetails' {} a -> s {networkInterfaceId = a} :: AwsEc2InstanceNetworkInterfacesDetails)

instance
  Data.FromJSON
    AwsEc2InstanceNetworkInterfacesDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2InstanceNetworkInterfacesDetails"
      ( \x ->
          AwsEc2InstanceNetworkInterfacesDetails'
            Prelude.<$> (x Data..:? "NetworkInterfaceId")
      )

instance
  Prelude.Hashable
    AwsEc2InstanceNetworkInterfacesDetails
  where
  hashWithSalt
    _salt
    AwsEc2InstanceNetworkInterfacesDetails' {..} =
      _salt `Prelude.hashWithSalt` networkInterfaceId

instance
  Prelude.NFData
    AwsEc2InstanceNetworkInterfacesDetails
  where
  rnf AwsEc2InstanceNetworkInterfacesDetails' {..} =
    Prelude.rnf networkInterfaceId

instance
  Data.ToJSON
    AwsEc2InstanceNetworkInterfacesDetails
  where
  toJSON AwsEc2InstanceNetworkInterfacesDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NetworkInterfaceId" Data..=)
              Prelude.<$> networkInterfaceId
          ]
      )
