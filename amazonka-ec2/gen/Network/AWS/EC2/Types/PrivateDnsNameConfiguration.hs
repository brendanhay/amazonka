{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.PrivateDnsNameConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrivateDnsNameConfiguration where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DnsNameState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the private DNS name for the service endpoint. For
-- more information about these parameters, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/ndpoint-services-dns-validation.html VPC Endpoint Service Private DNS Name Verification>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- /See:/ 'newPrivateDnsNameConfiguration' smart constructor.
data PrivateDnsNameConfiguration = PrivateDnsNameConfiguration'
  { -- | The verification state of the VPC endpoint service.
    --
    -- >Consumers of the endpoint service can use the private name only when
    -- the state is @verified@.
    state :: Prelude.Maybe DnsNameState,
    -- | The name of the record subdomain the service provider needs to create.
    -- The service provider adds the @value@ text to the @name@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The value the service provider adds to the private DNS name domain
    -- record before verification.
    value :: Prelude.Maybe Prelude.Text,
    -- | The endpoint service verification type, for example TXT.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PrivateDnsNameConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'privateDnsNameConfiguration_state' - The verification state of the VPC endpoint service.
--
-- >Consumers of the endpoint service can use the private name only when
-- the state is @verified@.
--
-- 'name', 'privateDnsNameConfiguration_name' - The name of the record subdomain the service provider needs to create.
-- The service provider adds the @value@ text to the @name@.
--
-- 'value', 'privateDnsNameConfiguration_value' - The value the service provider adds to the private DNS name domain
-- record before verification.
--
-- 'type'', 'privateDnsNameConfiguration_type' - The endpoint service verification type, for example TXT.
newPrivateDnsNameConfiguration ::
  PrivateDnsNameConfiguration
newPrivateDnsNameConfiguration =
  PrivateDnsNameConfiguration'
    { state =
        Prelude.Nothing,
      name = Prelude.Nothing,
      value = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The verification state of the VPC endpoint service.
--
-- >Consumers of the endpoint service can use the private name only when
-- the state is @verified@.
privateDnsNameConfiguration_state :: Lens.Lens' PrivateDnsNameConfiguration (Prelude.Maybe DnsNameState)
privateDnsNameConfiguration_state = Lens.lens (\PrivateDnsNameConfiguration' {state} -> state) (\s@PrivateDnsNameConfiguration' {} a -> s {state = a} :: PrivateDnsNameConfiguration)

-- | The name of the record subdomain the service provider needs to create.
-- The service provider adds the @value@ text to the @name@.
privateDnsNameConfiguration_name :: Lens.Lens' PrivateDnsNameConfiguration (Prelude.Maybe Prelude.Text)
privateDnsNameConfiguration_name = Lens.lens (\PrivateDnsNameConfiguration' {name} -> name) (\s@PrivateDnsNameConfiguration' {} a -> s {name = a} :: PrivateDnsNameConfiguration)

-- | The value the service provider adds to the private DNS name domain
-- record before verification.
privateDnsNameConfiguration_value :: Lens.Lens' PrivateDnsNameConfiguration (Prelude.Maybe Prelude.Text)
privateDnsNameConfiguration_value = Lens.lens (\PrivateDnsNameConfiguration' {value} -> value) (\s@PrivateDnsNameConfiguration' {} a -> s {value = a} :: PrivateDnsNameConfiguration)

-- | The endpoint service verification type, for example TXT.
privateDnsNameConfiguration_type :: Lens.Lens' PrivateDnsNameConfiguration (Prelude.Maybe Prelude.Text)
privateDnsNameConfiguration_type = Lens.lens (\PrivateDnsNameConfiguration' {type'} -> type') (\s@PrivateDnsNameConfiguration' {} a -> s {type' = a} :: PrivateDnsNameConfiguration)

instance Prelude.FromXML PrivateDnsNameConfiguration where
  parseXML x =
    PrivateDnsNameConfiguration'
      Prelude.<$> (x Prelude..@? "state")
      Prelude.<*> (x Prelude..@? "name")
      Prelude.<*> (x Prelude..@? "value")
      Prelude.<*> (x Prelude..@? "type")

instance Prelude.Hashable PrivateDnsNameConfiguration

instance Prelude.NFData PrivateDnsNameConfiguration
