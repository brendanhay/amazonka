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
-- Module      : Amazonka.StorageGateway.Types.EndpointNetworkConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.EndpointNetworkConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies network configuration information for the gateway associated
-- with the Amazon FSx file system.
--
-- /See:/ 'newEndpointNetworkConfiguration' smart constructor.
data EndpointNetworkConfiguration = EndpointNetworkConfiguration'
  { -- | A list of gateway IP addresses on which the associated Amazon FSx file
    -- system is available.
    --
    -- If multiple file systems are associated with this gateway, this field is
    -- required.
    ipAddresses :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointNetworkConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddresses', 'endpointNetworkConfiguration_ipAddresses' - A list of gateway IP addresses on which the associated Amazon FSx file
-- system is available.
--
-- If multiple file systems are associated with this gateway, this field is
-- required.
newEndpointNetworkConfiguration ::
  EndpointNetworkConfiguration
newEndpointNetworkConfiguration =
  EndpointNetworkConfiguration'
    { ipAddresses =
        Prelude.Nothing
    }

-- | A list of gateway IP addresses on which the associated Amazon FSx file
-- system is available.
--
-- If multiple file systems are associated with this gateway, this field is
-- required.
endpointNetworkConfiguration_ipAddresses :: Lens.Lens' EndpointNetworkConfiguration (Prelude.Maybe [Prelude.Text])
endpointNetworkConfiguration_ipAddresses = Lens.lens (\EndpointNetworkConfiguration' {ipAddresses} -> ipAddresses) (\s@EndpointNetworkConfiguration' {} a -> s {ipAddresses = a} :: EndpointNetworkConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON EndpointNetworkConfiguration where
  parseJSON =
    Data.withObject
      "EndpointNetworkConfiguration"
      ( \x ->
          EndpointNetworkConfiguration'
            Prelude.<$> (x Data..:? "IpAddresses" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    EndpointNetworkConfiguration
  where
  hashWithSalt _salt EndpointNetworkConfiguration' {..} =
    _salt `Prelude.hashWithSalt` ipAddresses

instance Prelude.NFData EndpointNetworkConfiguration where
  rnf EndpointNetworkConfiguration' {..} =
    Prelude.rnf ipAddresses

instance Data.ToJSON EndpointNetworkConfiguration where
  toJSON EndpointNetworkConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("IpAddresses" Data..=) Prelude.<$> ipAddresses]
      )
