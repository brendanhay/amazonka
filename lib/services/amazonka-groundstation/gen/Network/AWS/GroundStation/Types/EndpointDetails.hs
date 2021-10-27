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
-- Module      : Network.AWS.GroundStation.Types.EndpointDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GroundStation.Types.EndpointDetails where

import qualified Network.AWS.Core as Core
import Network.AWS.GroundStation.Types.DataflowEndpoint
import Network.AWS.GroundStation.Types.SecurityDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the endpoint details.
--
-- /See:/ 'newEndpointDetails' smart constructor.
data EndpointDetails = EndpointDetails'
  { -- | Endpoint security details.
    securityDetails :: Prelude.Maybe SecurityDetails,
    -- | A dataflow endpoint.
    endpoint :: Prelude.Maybe DataflowEndpoint
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
-- 'securityDetails', 'endpointDetails_securityDetails' - Endpoint security details.
--
-- 'endpoint', 'endpointDetails_endpoint' - A dataflow endpoint.
newEndpointDetails ::
  EndpointDetails
newEndpointDetails =
  EndpointDetails'
    { securityDetails = Prelude.Nothing,
      endpoint = Prelude.Nothing
    }

-- | Endpoint security details.
endpointDetails_securityDetails :: Lens.Lens' EndpointDetails (Prelude.Maybe SecurityDetails)
endpointDetails_securityDetails = Lens.lens (\EndpointDetails' {securityDetails} -> securityDetails) (\s@EndpointDetails' {} a -> s {securityDetails = a} :: EndpointDetails)

-- | A dataflow endpoint.
endpointDetails_endpoint :: Lens.Lens' EndpointDetails (Prelude.Maybe DataflowEndpoint)
endpointDetails_endpoint = Lens.lens (\EndpointDetails' {endpoint} -> endpoint) (\s@EndpointDetails' {} a -> s {endpoint = a} :: EndpointDetails)

instance Core.FromJSON EndpointDetails where
  parseJSON =
    Core.withObject
      "EndpointDetails"
      ( \x ->
          EndpointDetails'
            Prelude.<$> (x Core..:? "securityDetails")
            Prelude.<*> (x Core..:? "endpoint")
      )

instance Prelude.Hashable EndpointDetails

instance Prelude.NFData EndpointDetails

instance Core.ToJSON EndpointDetails where
  toJSON EndpointDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("securityDetails" Core..=)
              Prelude.<$> securityDetails,
            ("endpoint" Core..=) Prelude.<$> endpoint
          ]
      )
