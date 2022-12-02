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
-- Module      : Amazonka.EC2.Types.PrivateDnsNameConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PrivateDnsNameConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.DnsNameState
import qualified Amazonka.Prelude as Prelude

-- | Information about the private DNS name for the service endpoint.
--
-- /See:/ 'newPrivateDnsNameConfiguration' smart constructor.
data PrivateDnsNameConfiguration = PrivateDnsNameConfiguration'
  { -- | The name of the record subdomain the service provider needs to create.
    -- The service provider adds the @value@ text to the @name@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The endpoint service verification type, for example TXT.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The verification state of the VPC endpoint service.
    --
    -- >Consumers of the endpoint service can use the private name only when
    -- the state is @verified@.
    state :: Prelude.Maybe DnsNameState,
    -- | The value the service provider adds to the private DNS name domain
    -- record before verification.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrivateDnsNameConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'privateDnsNameConfiguration_name' - The name of the record subdomain the service provider needs to create.
-- The service provider adds the @value@ text to the @name@.
--
-- 'type'', 'privateDnsNameConfiguration_type' - The endpoint service verification type, for example TXT.
--
-- 'state', 'privateDnsNameConfiguration_state' - The verification state of the VPC endpoint service.
--
-- >Consumers of the endpoint service can use the private name only when
-- the state is @verified@.
--
-- 'value', 'privateDnsNameConfiguration_value' - The value the service provider adds to the private DNS name domain
-- record before verification.
newPrivateDnsNameConfiguration ::
  PrivateDnsNameConfiguration
newPrivateDnsNameConfiguration =
  PrivateDnsNameConfiguration'
    { name =
        Prelude.Nothing,
      type' = Prelude.Nothing,
      state = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the record subdomain the service provider needs to create.
-- The service provider adds the @value@ text to the @name@.
privateDnsNameConfiguration_name :: Lens.Lens' PrivateDnsNameConfiguration (Prelude.Maybe Prelude.Text)
privateDnsNameConfiguration_name = Lens.lens (\PrivateDnsNameConfiguration' {name} -> name) (\s@PrivateDnsNameConfiguration' {} a -> s {name = a} :: PrivateDnsNameConfiguration)

-- | The endpoint service verification type, for example TXT.
privateDnsNameConfiguration_type :: Lens.Lens' PrivateDnsNameConfiguration (Prelude.Maybe Prelude.Text)
privateDnsNameConfiguration_type = Lens.lens (\PrivateDnsNameConfiguration' {type'} -> type') (\s@PrivateDnsNameConfiguration' {} a -> s {type' = a} :: PrivateDnsNameConfiguration)

-- | The verification state of the VPC endpoint service.
--
-- >Consumers of the endpoint service can use the private name only when
-- the state is @verified@.
privateDnsNameConfiguration_state :: Lens.Lens' PrivateDnsNameConfiguration (Prelude.Maybe DnsNameState)
privateDnsNameConfiguration_state = Lens.lens (\PrivateDnsNameConfiguration' {state} -> state) (\s@PrivateDnsNameConfiguration' {} a -> s {state = a} :: PrivateDnsNameConfiguration)

-- | The value the service provider adds to the private DNS name domain
-- record before verification.
privateDnsNameConfiguration_value :: Lens.Lens' PrivateDnsNameConfiguration (Prelude.Maybe Prelude.Text)
privateDnsNameConfiguration_value = Lens.lens (\PrivateDnsNameConfiguration' {value} -> value) (\s@PrivateDnsNameConfiguration' {} a -> s {value = a} :: PrivateDnsNameConfiguration)

instance Data.FromXML PrivateDnsNameConfiguration where
  parseXML x =
    PrivateDnsNameConfiguration'
      Prelude.<$> (x Data..@? "name")
      Prelude.<*> (x Data..@? "type")
      Prelude.<*> (x Data..@? "state")
      Prelude.<*> (x Data..@? "value")

instance Prelude.Hashable PrivateDnsNameConfiguration where
  hashWithSalt _salt PrivateDnsNameConfiguration' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` value

instance Prelude.NFData PrivateDnsNameConfiguration where
  rnf PrivateDnsNameConfiguration' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf value
