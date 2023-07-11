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
-- Module      : Amazonka.NetworkFirewall.Types.SubnetMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.SubnetMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The ID for a subnet that you want to associate with the firewall. This
-- is used with CreateFirewall and AssociateSubnets. Network Firewall
-- creates an instance of the associated firewall in each subnet that you
-- specify, to filter traffic in the subnet\'s Availability Zone.
--
-- /See:/ 'newSubnetMapping' smart constructor.
data SubnetMapping = SubnetMapping'
  { -- | The unique identifier for the subnet.
    subnetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubnetMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetId', 'subnetMapping_subnetId' - The unique identifier for the subnet.
newSubnetMapping ::
  -- | 'subnetId'
  Prelude.Text ->
  SubnetMapping
newSubnetMapping pSubnetId_ =
  SubnetMapping' {subnetId = pSubnetId_}

-- | The unique identifier for the subnet.
subnetMapping_subnetId :: Lens.Lens' SubnetMapping Prelude.Text
subnetMapping_subnetId = Lens.lens (\SubnetMapping' {subnetId} -> subnetId) (\s@SubnetMapping' {} a -> s {subnetId = a} :: SubnetMapping)

instance Data.FromJSON SubnetMapping where
  parseJSON =
    Data.withObject
      "SubnetMapping"
      ( \x ->
          SubnetMapping' Prelude.<$> (x Data..: "SubnetId")
      )

instance Prelude.Hashable SubnetMapping where
  hashWithSalt _salt SubnetMapping' {..} =
    _salt `Prelude.hashWithSalt` subnetId

instance Prelude.NFData SubnetMapping where
  rnf SubnetMapping' {..} = Prelude.rnf subnetId

instance Data.ToJSON SubnetMapping where
  toJSON SubnetMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("SubnetId" Data..= subnetId)]
      )
