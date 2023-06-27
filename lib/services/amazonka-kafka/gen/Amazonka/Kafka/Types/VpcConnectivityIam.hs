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
-- Module      : Amazonka.Kafka.Types.VpcConnectivityIam
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.VpcConnectivityIam where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details for IAM access control for VPC connectivity.
--
-- /See:/ 'newVpcConnectivityIam' smart constructor.
data VpcConnectivityIam = VpcConnectivityIam'
  { -- | SASL\/IAM authentication is on or off for VPC connectivity.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcConnectivityIam' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'vpcConnectivityIam_enabled' - SASL\/IAM authentication is on or off for VPC connectivity.
newVpcConnectivityIam ::
  VpcConnectivityIam
newVpcConnectivityIam =
  VpcConnectivityIam' {enabled = Prelude.Nothing}

-- | SASL\/IAM authentication is on or off for VPC connectivity.
vpcConnectivityIam_enabled :: Lens.Lens' VpcConnectivityIam (Prelude.Maybe Prelude.Bool)
vpcConnectivityIam_enabled = Lens.lens (\VpcConnectivityIam' {enabled} -> enabled) (\s@VpcConnectivityIam' {} a -> s {enabled = a} :: VpcConnectivityIam)

instance Data.FromJSON VpcConnectivityIam where
  parseJSON =
    Data.withObject
      "VpcConnectivityIam"
      ( \x ->
          VpcConnectivityIam'
            Prelude.<$> (x Data..:? "enabled")
      )

instance Prelude.Hashable VpcConnectivityIam where
  hashWithSalt _salt VpcConnectivityIam' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData VpcConnectivityIam where
  rnf VpcConnectivityIam' {..} = Prelude.rnf enabled

instance Data.ToJSON VpcConnectivityIam where
  toJSON VpcConnectivityIam' {..} =
    Data.object
      ( Prelude.catMaybes
          [("enabled" Data..=) Prelude.<$> enabled]
      )
