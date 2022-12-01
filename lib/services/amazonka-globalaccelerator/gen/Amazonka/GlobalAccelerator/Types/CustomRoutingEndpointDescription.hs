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
-- Module      : Amazonka.GlobalAccelerator.Types.CustomRoutingEndpointDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.CustomRoutingEndpointDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A complex type for an endpoint for a custom routing accelerator. Each
-- endpoint group can include one or more endpoints, which are virtual
-- private cloud (VPC) subnets.
--
-- /See:/ 'newCustomRoutingEndpointDescription' smart constructor.
data CustomRoutingEndpointDescription = CustomRoutingEndpointDescription'
  { -- | An ID for the endpoint. For custom routing accelerators, this is the
    -- virtual private cloud (VPC) subnet ID.
    endpointId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomRoutingEndpointDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointId', 'customRoutingEndpointDescription_endpointId' - An ID for the endpoint. For custom routing accelerators, this is the
-- virtual private cloud (VPC) subnet ID.
newCustomRoutingEndpointDescription ::
  CustomRoutingEndpointDescription
newCustomRoutingEndpointDescription =
  CustomRoutingEndpointDescription'
    { endpointId =
        Prelude.Nothing
    }

-- | An ID for the endpoint. For custom routing accelerators, this is the
-- virtual private cloud (VPC) subnet ID.
customRoutingEndpointDescription_endpointId :: Lens.Lens' CustomRoutingEndpointDescription (Prelude.Maybe Prelude.Text)
customRoutingEndpointDescription_endpointId = Lens.lens (\CustomRoutingEndpointDescription' {endpointId} -> endpointId) (\s@CustomRoutingEndpointDescription' {} a -> s {endpointId = a} :: CustomRoutingEndpointDescription)

instance
  Core.FromJSON
    CustomRoutingEndpointDescription
  where
  parseJSON =
    Core.withObject
      "CustomRoutingEndpointDescription"
      ( \x ->
          CustomRoutingEndpointDescription'
            Prelude.<$> (x Core..:? "EndpointId")
      )

instance
  Prelude.Hashable
    CustomRoutingEndpointDescription
  where
  hashWithSalt
    _salt
    CustomRoutingEndpointDescription' {..} =
      _salt `Prelude.hashWithSalt` endpointId

instance
  Prelude.NFData
    CustomRoutingEndpointDescription
  where
  rnf CustomRoutingEndpointDescription' {..} =
    Prelude.rnf endpointId
