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
-- Module      : Amazonka.Kafka.Types.VpcConnectivityScram
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.VpcConnectivityScram where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details for SASL\/SCRAM client authentication for VPC connectivity.
--
-- /See:/ 'newVpcConnectivityScram' smart constructor.
data VpcConnectivityScram = VpcConnectivityScram'
  { -- | SASL\/SCRAM authentication is on or off for VPC connectivity.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcConnectivityScram' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'vpcConnectivityScram_enabled' - SASL\/SCRAM authentication is on or off for VPC connectivity.
newVpcConnectivityScram ::
  VpcConnectivityScram
newVpcConnectivityScram =
  VpcConnectivityScram' {enabled = Prelude.Nothing}

-- | SASL\/SCRAM authentication is on or off for VPC connectivity.
vpcConnectivityScram_enabled :: Lens.Lens' VpcConnectivityScram (Prelude.Maybe Prelude.Bool)
vpcConnectivityScram_enabled = Lens.lens (\VpcConnectivityScram' {enabled} -> enabled) (\s@VpcConnectivityScram' {} a -> s {enabled = a} :: VpcConnectivityScram)

instance Data.FromJSON VpcConnectivityScram where
  parseJSON =
    Data.withObject
      "VpcConnectivityScram"
      ( \x ->
          VpcConnectivityScram'
            Prelude.<$> (x Data..:? "enabled")
      )

instance Prelude.Hashable VpcConnectivityScram where
  hashWithSalt _salt VpcConnectivityScram' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData VpcConnectivityScram where
  rnf VpcConnectivityScram' {..} = Prelude.rnf enabled

instance Data.ToJSON VpcConnectivityScram where
  toJSON VpcConnectivityScram' {..} =
    Data.object
      ( Prelude.catMaybes
          [("enabled" Data..=) Prelude.<$> enabled]
      )
