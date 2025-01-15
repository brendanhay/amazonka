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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2VpnConnectionOptionsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2VpnConnectionOptionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEc2VpnConnectionOptionsTunnelOptionsDetails

-- | VPN connection options.
--
-- /See:/ 'newAwsEc2VpnConnectionOptionsDetails' smart constructor.
data AwsEc2VpnConnectionOptionsDetails = AwsEc2VpnConnectionOptionsDetails'
  { -- | Whether the VPN connection uses static routes only.
    staticRoutesOnly :: Prelude.Maybe Prelude.Bool,
    -- | The VPN tunnel options.
    tunnelOptions :: Prelude.Maybe [AwsEc2VpnConnectionOptionsTunnelOptionsDetails]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2VpnConnectionOptionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'staticRoutesOnly', 'awsEc2VpnConnectionOptionsDetails_staticRoutesOnly' - Whether the VPN connection uses static routes only.
--
-- 'tunnelOptions', 'awsEc2VpnConnectionOptionsDetails_tunnelOptions' - The VPN tunnel options.
newAwsEc2VpnConnectionOptionsDetails ::
  AwsEc2VpnConnectionOptionsDetails
newAwsEc2VpnConnectionOptionsDetails =
  AwsEc2VpnConnectionOptionsDetails'
    { staticRoutesOnly =
        Prelude.Nothing,
      tunnelOptions = Prelude.Nothing
    }

-- | Whether the VPN connection uses static routes only.
awsEc2VpnConnectionOptionsDetails_staticRoutesOnly :: Lens.Lens' AwsEc2VpnConnectionOptionsDetails (Prelude.Maybe Prelude.Bool)
awsEc2VpnConnectionOptionsDetails_staticRoutesOnly = Lens.lens (\AwsEc2VpnConnectionOptionsDetails' {staticRoutesOnly} -> staticRoutesOnly) (\s@AwsEc2VpnConnectionOptionsDetails' {} a -> s {staticRoutesOnly = a} :: AwsEc2VpnConnectionOptionsDetails)

-- | The VPN tunnel options.
awsEc2VpnConnectionOptionsDetails_tunnelOptions :: Lens.Lens' AwsEc2VpnConnectionOptionsDetails (Prelude.Maybe [AwsEc2VpnConnectionOptionsTunnelOptionsDetails])
awsEc2VpnConnectionOptionsDetails_tunnelOptions = Lens.lens (\AwsEc2VpnConnectionOptionsDetails' {tunnelOptions} -> tunnelOptions) (\s@AwsEc2VpnConnectionOptionsDetails' {} a -> s {tunnelOptions = a} :: AwsEc2VpnConnectionOptionsDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AwsEc2VpnConnectionOptionsDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2VpnConnectionOptionsDetails"
      ( \x ->
          AwsEc2VpnConnectionOptionsDetails'
            Prelude.<$> (x Data..:? "StaticRoutesOnly")
            Prelude.<*> (x Data..:? "TunnelOptions" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsEc2VpnConnectionOptionsDetails
  where
  hashWithSalt
    _salt
    AwsEc2VpnConnectionOptionsDetails' {..} =
      _salt
        `Prelude.hashWithSalt` staticRoutesOnly
        `Prelude.hashWithSalt` tunnelOptions

instance
  Prelude.NFData
    AwsEc2VpnConnectionOptionsDetails
  where
  rnf AwsEc2VpnConnectionOptionsDetails' {..} =
    Prelude.rnf staticRoutesOnly `Prelude.seq`
      Prelude.rnf tunnelOptions

instance
  Data.ToJSON
    AwsEc2VpnConnectionOptionsDetails
  where
  toJSON AwsEc2VpnConnectionOptionsDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("StaticRoutesOnly" Data..=)
              Prelude.<$> staticRoutesOnly,
            ("TunnelOptions" Data..=) Prelude.<$> tunnelOptions
          ]
      )
