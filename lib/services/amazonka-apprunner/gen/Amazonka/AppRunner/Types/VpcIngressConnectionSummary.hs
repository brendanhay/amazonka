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
-- Module      : Amazonka.AppRunner.Types.VpcIngressConnectionSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.VpcIngressConnectionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides summary information about an VPC Ingress Connection, which
-- includes its VPC Ingress Connection ARN and its associated Service ARN.
--
-- /See:/ 'newVpcIngressConnectionSummary' smart constructor.
data VpcIngressConnectionSummary = VpcIngressConnectionSummary'
  { -- | The Amazon Resource Name (ARN) of the service associated with the VPC
    -- Ingress Connection.
    serviceArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the VPC Ingress Connection.
    vpcIngressConnectionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcIngressConnectionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceArn', 'vpcIngressConnectionSummary_serviceArn' - The Amazon Resource Name (ARN) of the service associated with the VPC
-- Ingress Connection.
--
-- 'vpcIngressConnectionArn', 'vpcIngressConnectionSummary_vpcIngressConnectionArn' - The Amazon Resource Name (ARN) of the VPC Ingress Connection.
newVpcIngressConnectionSummary ::
  VpcIngressConnectionSummary
newVpcIngressConnectionSummary =
  VpcIngressConnectionSummary'
    { serviceArn =
        Prelude.Nothing,
      vpcIngressConnectionArn = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the service associated with the VPC
-- Ingress Connection.
vpcIngressConnectionSummary_serviceArn :: Lens.Lens' VpcIngressConnectionSummary (Prelude.Maybe Prelude.Text)
vpcIngressConnectionSummary_serviceArn = Lens.lens (\VpcIngressConnectionSummary' {serviceArn} -> serviceArn) (\s@VpcIngressConnectionSummary' {} a -> s {serviceArn = a} :: VpcIngressConnectionSummary)

-- | The Amazon Resource Name (ARN) of the VPC Ingress Connection.
vpcIngressConnectionSummary_vpcIngressConnectionArn :: Lens.Lens' VpcIngressConnectionSummary (Prelude.Maybe Prelude.Text)
vpcIngressConnectionSummary_vpcIngressConnectionArn = Lens.lens (\VpcIngressConnectionSummary' {vpcIngressConnectionArn} -> vpcIngressConnectionArn) (\s@VpcIngressConnectionSummary' {} a -> s {vpcIngressConnectionArn = a} :: VpcIngressConnectionSummary)

instance Data.FromJSON VpcIngressConnectionSummary where
  parseJSON =
    Data.withObject
      "VpcIngressConnectionSummary"
      ( \x ->
          VpcIngressConnectionSummary'
            Prelude.<$> (x Data..:? "ServiceArn")
            Prelude.<*> (x Data..:? "VpcIngressConnectionArn")
      )

instance Prelude.Hashable VpcIngressConnectionSummary where
  hashWithSalt _salt VpcIngressConnectionSummary' {..} =
    _salt `Prelude.hashWithSalt` serviceArn
      `Prelude.hashWithSalt` vpcIngressConnectionArn

instance Prelude.NFData VpcIngressConnectionSummary where
  rnf VpcIngressConnectionSummary' {..} =
    Prelude.rnf serviceArn
      `Prelude.seq` Prelude.rnf vpcIngressConnectionArn
