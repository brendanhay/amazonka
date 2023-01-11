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
-- Module      : Amazonka.GroundStation.Types.EndpointDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.EndpointDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.DataflowEndpoint
import Amazonka.GroundStation.Types.SecurityDetails
import qualified Amazonka.Prelude as Prelude

-- | Information about the endpoint details.
--
-- /See:/ 'newEndpointDetails' smart constructor.
data EndpointDetails = EndpointDetails'
  { -- | A dataflow endpoint.
    endpoint :: Prelude.Maybe DataflowEndpoint,
    -- | Endpoint security details including a list of subnets, a list of
    -- security groups and a role to connect streams to instances.
    securityDetails :: Prelude.Maybe SecurityDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoint', 'endpointDetails_endpoint' - A dataflow endpoint.
--
-- 'securityDetails', 'endpointDetails_securityDetails' - Endpoint security details including a list of subnets, a list of
-- security groups and a role to connect streams to instances.
newEndpointDetails ::
  EndpointDetails
newEndpointDetails =
  EndpointDetails'
    { endpoint = Prelude.Nothing,
      securityDetails = Prelude.Nothing
    }

-- | A dataflow endpoint.
endpointDetails_endpoint :: Lens.Lens' EndpointDetails (Prelude.Maybe DataflowEndpoint)
endpointDetails_endpoint = Lens.lens (\EndpointDetails' {endpoint} -> endpoint) (\s@EndpointDetails' {} a -> s {endpoint = a} :: EndpointDetails)

-- | Endpoint security details including a list of subnets, a list of
-- security groups and a role to connect streams to instances.
endpointDetails_securityDetails :: Lens.Lens' EndpointDetails (Prelude.Maybe SecurityDetails)
endpointDetails_securityDetails = Lens.lens (\EndpointDetails' {securityDetails} -> securityDetails) (\s@EndpointDetails' {} a -> s {securityDetails = a} :: EndpointDetails)

instance Data.FromJSON EndpointDetails where
  parseJSON =
    Data.withObject
      "EndpointDetails"
      ( \x ->
          EndpointDetails'
            Prelude.<$> (x Data..:? "endpoint")
            Prelude.<*> (x Data..:? "securityDetails")
      )

instance Prelude.Hashable EndpointDetails where
  hashWithSalt _salt EndpointDetails' {..} =
    _salt `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` securityDetails

instance Prelude.NFData EndpointDetails where
  rnf EndpointDetails' {..} =
    Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf securityDetails

instance Data.ToJSON EndpointDetails where
  toJSON EndpointDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("endpoint" Data..=) Prelude.<$> endpoint,
            ("securityDetails" Data..=)
              Prelude.<$> securityDetails
          ]
      )
