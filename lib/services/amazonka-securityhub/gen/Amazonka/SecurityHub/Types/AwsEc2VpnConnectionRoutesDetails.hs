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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2VpnConnectionRoutesDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2VpnConnectionRoutesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A static routes associated with the VPN connection.
--
-- /See:/ 'newAwsEc2VpnConnectionRoutesDetails' smart constructor.
data AwsEc2VpnConnectionRoutesDetails = AwsEc2VpnConnectionRoutesDetails'
  { -- | The CIDR block associated with the local subnet of the customer data
    -- center.
    destinationCidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The current state of the static route.
    state :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2VpnConnectionRoutesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationCidrBlock', 'awsEc2VpnConnectionRoutesDetails_destinationCidrBlock' - The CIDR block associated with the local subnet of the customer data
-- center.
--
-- 'state', 'awsEc2VpnConnectionRoutesDetails_state' - The current state of the static route.
newAwsEc2VpnConnectionRoutesDetails ::
  AwsEc2VpnConnectionRoutesDetails
newAwsEc2VpnConnectionRoutesDetails =
  AwsEc2VpnConnectionRoutesDetails'
    { destinationCidrBlock =
        Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The CIDR block associated with the local subnet of the customer data
-- center.
awsEc2VpnConnectionRoutesDetails_destinationCidrBlock :: Lens.Lens' AwsEc2VpnConnectionRoutesDetails (Prelude.Maybe Prelude.Text)
awsEc2VpnConnectionRoutesDetails_destinationCidrBlock = Lens.lens (\AwsEc2VpnConnectionRoutesDetails' {destinationCidrBlock} -> destinationCidrBlock) (\s@AwsEc2VpnConnectionRoutesDetails' {} a -> s {destinationCidrBlock = a} :: AwsEc2VpnConnectionRoutesDetails)

-- | The current state of the static route.
awsEc2VpnConnectionRoutesDetails_state :: Lens.Lens' AwsEc2VpnConnectionRoutesDetails (Prelude.Maybe Prelude.Text)
awsEc2VpnConnectionRoutesDetails_state = Lens.lens (\AwsEc2VpnConnectionRoutesDetails' {state} -> state) (\s@AwsEc2VpnConnectionRoutesDetails' {} a -> s {state = a} :: AwsEc2VpnConnectionRoutesDetails)

instance
  Data.FromJSON
    AwsEc2VpnConnectionRoutesDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2VpnConnectionRoutesDetails"
      ( \x ->
          AwsEc2VpnConnectionRoutesDetails'
            Prelude.<$> (x Data..:? "DestinationCidrBlock")
            Prelude.<*> (x Data..:? "State")
      )

instance
  Prelude.Hashable
    AwsEc2VpnConnectionRoutesDetails
  where
  hashWithSalt
    _salt
    AwsEc2VpnConnectionRoutesDetails' {..} =
      _salt
        `Prelude.hashWithSalt` destinationCidrBlock
        `Prelude.hashWithSalt` state

instance
  Prelude.NFData
    AwsEc2VpnConnectionRoutesDetails
  where
  rnf AwsEc2VpnConnectionRoutesDetails' {..} =
    Prelude.rnf destinationCidrBlock
      `Prelude.seq` Prelude.rnf state

instance Data.ToJSON AwsEc2VpnConnectionRoutesDetails where
  toJSON AwsEc2VpnConnectionRoutesDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DestinationCidrBlock" Data..=)
              Prelude.<$> destinationCidrBlock,
            ("State" Data..=) Prelude.<$> state
          ]
      )
