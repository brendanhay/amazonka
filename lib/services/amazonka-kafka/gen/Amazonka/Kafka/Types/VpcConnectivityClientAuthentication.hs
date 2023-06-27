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
-- Module      : Amazonka.Kafka.Types.VpcConnectivityClientAuthentication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.VpcConnectivityClientAuthentication where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.VpcConnectivitySasl
import Amazonka.Kafka.Types.VpcConnectivityTls
import qualified Amazonka.Prelude as Prelude

-- | Includes all client authentication information for VPC connectivity.
--
-- /See:/ 'newVpcConnectivityClientAuthentication' smart constructor.
data VpcConnectivityClientAuthentication = VpcConnectivityClientAuthentication'
  { -- | SASL authentication type details for VPC connectivity.
    sasl :: Prelude.Maybe VpcConnectivitySasl,
    -- | TLS authentication type details for VPC connectivity.
    tls :: Prelude.Maybe VpcConnectivityTls
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcConnectivityClientAuthentication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sasl', 'vpcConnectivityClientAuthentication_sasl' - SASL authentication type details for VPC connectivity.
--
-- 'tls', 'vpcConnectivityClientAuthentication_tls' - TLS authentication type details for VPC connectivity.
newVpcConnectivityClientAuthentication ::
  VpcConnectivityClientAuthentication
newVpcConnectivityClientAuthentication =
  VpcConnectivityClientAuthentication'
    { sasl =
        Prelude.Nothing,
      tls = Prelude.Nothing
    }

-- | SASL authentication type details for VPC connectivity.
vpcConnectivityClientAuthentication_sasl :: Lens.Lens' VpcConnectivityClientAuthentication (Prelude.Maybe VpcConnectivitySasl)
vpcConnectivityClientAuthentication_sasl = Lens.lens (\VpcConnectivityClientAuthentication' {sasl} -> sasl) (\s@VpcConnectivityClientAuthentication' {} a -> s {sasl = a} :: VpcConnectivityClientAuthentication)

-- | TLS authentication type details for VPC connectivity.
vpcConnectivityClientAuthentication_tls :: Lens.Lens' VpcConnectivityClientAuthentication (Prelude.Maybe VpcConnectivityTls)
vpcConnectivityClientAuthentication_tls = Lens.lens (\VpcConnectivityClientAuthentication' {tls} -> tls) (\s@VpcConnectivityClientAuthentication' {} a -> s {tls = a} :: VpcConnectivityClientAuthentication)

instance
  Data.FromJSON
    VpcConnectivityClientAuthentication
  where
  parseJSON =
    Data.withObject
      "VpcConnectivityClientAuthentication"
      ( \x ->
          VpcConnectivityClientAuthentication'
            Prelude.<$> (x Data..:? "sasl")
            Prelude.<*> (x Data..:? "tls")
      )

instance
  Prelude.Hashable
    VpcConnectivityClientAuthentication
  where
  hashWithSalt
    _salt
    VpcConnectivityClientAuthentication' {..} =
      _salt
        `Prelude.hashWithSalt` sasl
        `Prelude.hashWithSalt` tls

instance
  Prelude.NFData
    VpcConnectivityClientAuthentication
  where
  rnf VpcConnectivityClientAuthentication' {..} =
    Prelude.rnf sasl `Prelude.seq` Prelude.rnf tls

instance
  Data.ToJSON
    VpcConnectivityClientAuthentication
  where
  toJSON VpcConnectivityClientAuthentication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("sasl" Data..=) Prelude.<$> sasl,
            ("tls" Data..=) Prelude.<$> tls
          ]
      )
