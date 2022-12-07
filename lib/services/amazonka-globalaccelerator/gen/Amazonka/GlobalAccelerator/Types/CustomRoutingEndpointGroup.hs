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
-- Module      : Amazonka.GlobalAccelerator.Types.CustomRoutingEndpointGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.CustomRoutingEndpointGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types.CustomRoutingDestinationDescription
import Amazonka.GlobalAccelerator.Types.CustomRoutingEndpointDescription
import qualified Amazonka.Prelude as Prelude

-- | A complex type for the endpoint group for a custom routing accelerator.
-- An Amazon Web Services Region can have only one endpoint group for a
-- specific listener.
--
-- /See:/ 'newCustomRoutingEndpointGroup' smart constructor.
data CustomRoutingEndpointGroup = CustomRoutingEndpointGroup'
  { -- | The Amazon Web Services Region where the endpoint group is located.
    endpointGroupRegion :: Prelude.Maybe Prelude.Text,
    -- | For a custom routing accelerator, describes the endpoints (virtual
    -- private cloud subnets) in an endpoint group to accept client traffic on.
    endpointDescriptions :: Prelude.Maybe [CustomRoutingEndpointDescription],
    -- | For a custom routing accelerator, describes the port range and protocol
    -- for all endpoints (virtual private cloud subnets) in an endpoint group
    -- to accept client traffic on.
    destinationDescriptions :: Prelude.Maybe [CustomRoutingDestinationDescription],
    -- | The Amazon Resource Name (ARN) of the endpoint group.
    endpointGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomRoutingEndpointGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointGroupRegion', 'customRoutingEndpointGroup_endpointGroupRegion' - The Amazon Web Services Region where the endpoint group is located.
--
-- 'endpointDescriptions', 'customRoutingEndpointGroup_endpointDescriptions' - For a custom routing accelerator, describes the endpoints (virtual
-- private cloud subnets) in an endpoint group to accept client traffic on.
--
-- 'destinationDescriptions', 'customRoutingEndpointGroup_destinationDescriptions' - For a custom routing accelerator, describes the port range and protocol
-- for all endpoints (virtual private cloud subnets) in an endpoint group
-- to accept client traffic on.
--
-- 'endpointGroupArn', 'customRoutingEndpointGroup_endpointGroupArn' - The Amazon Resource Name (ARN) of the endpoint group.
newCustomRoutingEndpointGroup ::
  CustomRoutingEndpointGroup
newCustomRoutingEndpointGroup =
  CustomRoutingEndpointGroup'
    { endpointGroupRegion =
        Prelude.Nothing,
      endpointDescriptions = Prelude.Nothing,
      destinationDescriptions = Prelude.Nothing,
      endpointGroupArn = Prelude.Nothing
    }

-- | The Amazon Web Services Region where the endpoint group is located.
customRoutingEndpointGroup_endpointGroupRegion :: Lens.Lens' CustomRoutingEndpointGroup (Prelude.Maybe Prelude.Text)
customRoutingEndpointGroup_endpointGroupRegion = Lens.lens (\CustomRoutingEndpointGroup' {endpointGroupRegion} -> endpointGroupRegion) (\s@CustomRoutingEndpointGroup' {} a -> s {endpointGroupRegion = a} :: CustomRoutingEndpointGroup)

-- | For a custom routing accelerator, describes the endpoints (virtual
-- private cloud subnets) in an endpoint group to accept client traffic on.
customRoutingEndpointGroup_endpointDescriptions :: Lens.Lens' CustomRoutingEndpointGroup (Prelude.Maybe [CustomRoutingEndpointDescription])
customRoutingEndpointGroup_endpointDescriptions = Lens.lens (\CustomRoutingEndpointGroup' {endpointDescriptions} -> endpointDescriptions) (\s@CustomRoutingEndpointGroup' {} a -> s {endpointDescriptions = a} :: CustomRoutingEndpointGroup) Prelude.. Lens.mapping Lens.coerced

-- | For a custom routing accelerator, describes the port range and protocol
-- for all endpoints (virtual private cloud subnets) in an endpoint group
-- to accept client traffic on.
customRoutingEndpointGroup_destinationDescriptions :: Lens.Lens' CustomRoutingEndpointGroup (Prelude.Maybe [CustomRoutingDestinationDescription])
customRoutingEndpointGroup_destinationDescriptions = Lens.lens (\CustomRoutingEndpointGroup' {destinationDescriptions} -> destinationDescriptions) (\s@CustomRoutingEndpointGroup' {} a -> s {destinationDescriptions = a} :: CustomRoutingEndpointGroup) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the endpoint group.
customRoutingEndpointGroup_endpointGroupArn :: Lens.Lens' CustomRoutingEndpointGroup (Prelude.Maybe Prelude.Text)
customRoutingEndpointGroup_endpointGroupArn = Lens.lens (\CustomRoutingEndpointGroup' {endpointGroupArn} -> endpointGroupArn) (\s@CustomRoutingEndpointGroup' {} a -> s {endpointGroupArn = a} :: CustomRoutingEndpointGroup)

instance Data.FromJSON CustomRoutingEndpointGroup where
  parseJSON =
    Data.withObject
      "CustomRoutingEndpointGroup"
      ( \x ->
          CustomRoutingEndpointGroup'
            Prelude.<$> (x Data..:? "EndpointGroupRegion")
            Prelude.<*> ( x Data..:? "EndpointDescriptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "DestinationDescriptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "EndpointGroupArn")
      )

instance Prelude.Hashable CustomRoutingEndpointGroup where
  hashWithSalt _salt CustomRoutingEndpointGroup' {..} =
    _salt `Prelude.hashWithSalt` endpointGroupRegion
      `Prelude.hashWithSalt` endpointDescriptions
      `Prelude.hashWithSalt` destinationDescriptions
      `Prelude.hashWithSalt` endpointGroupArn

instance Prelude.NFData CustomRoutingEndpointGroup where
  rnf CustomRoutingEndpointGroup' {..} =
    Prelude.rnf endpointGroupRegion
      `Prelude.seq` Prelude.rnf endpointDescriptions
      `Prelude.seq` Prelude.rnf destinationDescriptions
      `Prelude.seq` Prelude.rnf endpointGroupArn
