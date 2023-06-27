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
-- Module      : Amazonka.Kafka.Types.VpcConnectivitySasl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.VpcConnectivitySasl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.VpcConnectivityIam
import Amazonka.Kafka.Types.VpcConnectivityScram
import qualified Amazonka.Prelude as Prelude

-- | Details for SASL client authentication for VPC connectivity.
--
-- /See:/ 'newVpcConnectivitySasl' smart constructor.
data VpcConnectivitySasl = VpcConnectivitySasl'
  { -- | Details for SASL\/IAM client authentication for VPC connectivity.
    iam :: Prelude.Maybe VpcConnectivityIam,
    -- | Details for SASL\/SCRAM client authentication for VPC connectivity.
    scram :: Prelude.Maybe VpcConnectivityScram
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcConnectivitySasl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iam', 'vpcConnectivitySasl_iam' - Details for SASL\/IAM client authentication for VPC connectivity.
--
-- 'scram', 'vpcConnectivitySasl_scram' - Details for SASL\/SCRAM client authentication for VPC connectivity.
newVpcConnectivitySasl ::
  VpcConnectivitySasl
newVpcConnectivitySasl =
  VpcConnectivitySasl'
    { iam = Prelude.Nothing,
      scram = Prelude.Nothing
    }

-- | Details for SASL\/IAM client authentication for VPC connectivity.
vpcConnectivitySasl_iam :: Lens.Lens' VpcConnectivitySasl (Prelude.Maybe VpcConnectivityIam)
vpcConnectivitySasl_iam = Lens.lens (\VpcConnectivitySasl' {iam} -> iam) (\s@VpcConnectivitySasl' {} a -> s {iam = a} :: VpcConnectivitySasl)

-- | Details for SASL\/SCRAM client authentication for VPC connectivity.
vpcConnectivitySasl_scram :: Lens.Lens' VpcConnectivitySasl (Prelude.Maybe VpcConnectivityScram)
vpcConnectivitySasl_scram = Lens.lens (\VpcConnectivitySasl' {scram} -> scram) (\s@VpcConnectivitySasl' {} a -> s {scram = a} :: VpcConnectivitySasl)

instance Data.FromJSON VpcConnectivitySasl where
  parseJSON =
    Data.withObject
      "VpcConnectivitySasl"
      ( \x ->
          VpcConnectivitySasl'
            Prelude.<$> (x Data..:? "iam")
            Prelude.<*> (x Data..:? "scram")
      )

instance Prelude.Hashable VpcConnectivitySasl where
  hashWithSalt _salt VpcConnectivitySasl' {..} =
    _salt
      `Prelude.hashWithSalt` iam
      `Prelude.hashWithSalt` scram

instance Prelude.NFData VpcConnectivitySasl where
  rnf VpcConnectivitySasl' {..} =
    Prelude.rnf iam `Prelude.seq` Prelude.rnf scram

instance Data.ToJSON VpcConnectivitySasl where
  toJSON VpcConnectivitySasl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("iam" Data..=) Prelude.<$> iam,
            ("scram" Data..=) Prelude.<$> scram
          ]
      )
