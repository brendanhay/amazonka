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
-- Module      : Amazonka.Kafka.Types.VpcConnectivityTls
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.VpcConnectivityTls where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details for TLS client authentication for VPC connectivity.
--
-- /See:/ 'newVpcConnectivityTls' smart constructor.
data VpcConnectivityTls = VpcConnectivityTls'
  { -- | TLS authentication is on or off for VPC connectivity.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcConnectivityTls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'vpcConnectivityTls_enabled' - TLS authentication is on or off for VPC connectivity.
newVpcConnectivityTls ::
  VpcConnectivityTls
newVpcConnectivityTls =
  VpcConnectivityTls' {enabled = Prelude.Nothing}

-- | TLS authentication is on or off for VPC connectivity.
vpcConnectivityTls_enabled :: Lens.Lens' VpcConnectivityTls (Prelude.Maybe Prelude.Bool)
vpcConnectivityTls_enabled = Lens.lens (\VpcConnectivityTls' {enabled} -> enabled) (\s@VpcConnectivityTls' {} a -> s {enabled = a} :: VpcConnectivityTls)

instance Data.FromJSON VpcConnectivityTls where
  parseJSON =
    Data.withObject
      "VpcConnectivityTls"
      ( \x ->
          VpcConnectivityTls'
            Prelude.<$> (x Data..:? "enabled")
      )

instance Prelude.Hashable VpcConnectivityTls where
  hashWithSalt _salt VpcConnectivityTls' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData VpcConnectivityTls where
  rnf VpcConnectivityTls' {..} = Prelude.rnf enabled

instance Data.ToJSON VpcConnectivityTls where
  toJSON VpcConnectivityTls' {..} =
    Data.object
      ( Prelude.catMaybes
          [("enabled" Data..=) Prelude.<$> enabled]
      )
