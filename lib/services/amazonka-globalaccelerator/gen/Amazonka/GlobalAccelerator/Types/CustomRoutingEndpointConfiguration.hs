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
-- Module      : Amazonka.GlobalAccelerator.Types.CustomRoutingEndpointConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.CustomRoutingEndpointConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The list of endpoint objects. For custom routing, this is a list of
-- virtual private cloud (VPC) subnet IDs.
--
-- /See:/ 'newCustomRoutingEndpointConfiguration' smart constructor.
data CustomRoutingEndpointConfiguration = CustomRoutingEndpointConfiguration'
  { -- | An ID for the endpoint. For custom routing accelerators, this is the
    -- virtual private cloud (VPC) subnet ID.
    endpointId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomRoutingEndpointConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointId', 'customRoutingEndpointConfiguration_endpointId' - An ID for the endpoint. For custom routing accelerators, this is the
-- virtual private cloud (VPC) subnet ID.
newCustomRoutingEndpointConfiguration ::
  CustomRoutingEndpointConfiguration
newCustomRoutingEndpointConfiguration =
  CustomRoutingEndpointConfiguration'
    { endpointId =
        Prelude.Nothing
    }

-- | An ID for the endpoint. For custom routing accelerators, this is the
-- virtual private cloud (VPC) subnet ID.
customRoutingEndpointConfiguration_endpointId :: Lens.Lens' CustomRoutingEndpointConfiguration (Prelude.Maybe Prelude.Text)
customRoutingEndpointConfiguration_endpointId = Lens.lens (\CustomRoutingEndpointConfiguration' {endpointId} -> endpointId) (\s@CustomRoutingEndpointConfiguration' {} a -> s {endpointId = a} :: CustomRoutingEndpointConfiguration)

instance
  Prelude.Hashable
    CustomRoutingEndpointConfiguration
  where
  hashWithSalt
    _salt
    CustomRoutingEndpointConfiguration' {..} =
      _salt `Prelude.hashWithSalt` endpointId

instance
  Prelude.NFData
    CustomRoutingEndpointConfiguration
  where
  rnf CustomRoutingEndpointConfiguration' {..} =
    Prelude.rnf endpointId

instance
  Data.ToJSON
    CustomRoutingEndpointConfiguration
  where
  toJSON CustomRoutingEndpointConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("EndpointId" Data..=) Prelude.<$> endpointId]
      )
