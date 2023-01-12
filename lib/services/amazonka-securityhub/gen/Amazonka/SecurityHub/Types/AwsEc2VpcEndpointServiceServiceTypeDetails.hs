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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2VpcEndpointServiceServiceTypeDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2VpcEndpointServiceServiceTypeDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The service type information for a VPC endpoint service.
--
-- /See:/ 'newAwsEc2VpcEndpointServiceServiceTypeDetails' smart constructor.
data AwsEc2VpcEndpointServiceServiceTypeDetails = AwsEc2VpcEndpointServiceServiceTypeDetails'
  { -- | The type of service.
    serviceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2VpcEndpointServiceServiceTypeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceType', 'awsEc2VpcEndpointServiceServiceTypeDetails_serviceType' - The type of service.
newAwsEc2VpcEndpointServiceServiceTypeDetails ::
  AwsEc2VpcEndpointServiceServiceTypeDetails
newAwsEc2VpcEndpointServiceServiceTypeDetails =
  AwsEc2VpcEndpointServiceServiceTypeDetails'
    { serviceType =
        Prelude.Nothing
    }

-- | The type of service.
awsEc2VpcEndpointServiceServiceTypeDetails_serviceType :: Lens.Lens' AwsEc2VpcEndpointServiceServiceTypeDetails (Prelude.Maybe Prelude.Text)
awsEc2VpcEndpointServiceServiceTypeDetails_serviceType = Lens.lens (\AwsEc2VpcEndpointServiceServiceTypeDetails' {serviceType} -> serviceType) (\s@AwsEc2VpcEndpointServiceServiceTypeDetails' {} a -> s {serviceType = a} :: AwsEc2VpcEndpointServiceServiceTypeDetails)

instance
  Data.FromJSON
    AwsEc2VpcEndpointServiceServiceTypeDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2VpcEndpointServiceServiceTypeDetails"
      ( \x ->
          AwsEc2VpcEndpointServiceServiceTypeDetails'
            Prelude.<$> (x Data..:? "ServiceType")
      )

instance
  Prelude.Hashable
    AwsEc2VpcEndpointServiceServiceTypeDetails
  where
  hashWithSalt
    _salt
    AwsEc2VpcEndpointServiceServiceTypeDetails' {..} =
      _salt `Prelude.hashWithSalt` serviceType

instance
  Prelude.NFData
    AwsEc2VpcEndpointServiceServiceTypeDetails
  where
  rnf AwsEc2VpcEndpointServiceServiceTypeDetails' {..} =
    Prelude.rnf serviceType

instance
  Data.ToJSON
    AwsEc2VpcEndpointServiceServiceTypeDetails
  where
  toJSON
    AwsEc2VpcEndpointServiceServiceTypeDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [("ServiceType" Data..=) Prelude.<$> serviceType]
        )
