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
-- Module      : Amazonka.AppRunner.Types.IngressVpcConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.IngressVpcConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration of your VPC and the associated VPC endpoint. The VPC
-- endpoint is an Amazon Web Services PrivateLink resource that allows
-- access to your App Runner services from within an Amazon VPC.
--
-- /See:/ 'newIngressVpcConfiguration' smart constructor.
data IngressVpcConfiguration = IngressVpcConfiguration'
  { -- | The ID of the VPC endpoint that your App Runner service connects to.
    vpcEndpointId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC that is used for the VPC endpoint.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IngressVpcConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcEndpointId', 'ingressVpcConfiguration_vpcEndpointId' - The ID of the VPC endpoint that your App Runner service connects to.
--
-- 'vpcId', 'ingressVpcConfiguration_vpcId' - The ID of the VPC that is used for the VPC endpoint.
newIngressVpcConfiguration ::
  IngressVpcConfiguration
newIngressVpcConfiguration =
  IngressVpcConfiguration'
    { vpcEndpointId =
        Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The ID of the VPC endpoint that your App Runner service connects to.
ingressVpcConfiguration_vpcEndpointId :: Lens.Lens' IngressVpcConfiguration (Prelude.Maybe Prelude.Text)
ingressVpcConfiguration_vpcEndpointId = Lens.lens (\IngressVpcConfiguration' {vpcEndpointId} -> vpcEndpointId) (\s@IngressVpcConfiguration' {} a -> s {vpcEndpointId = a} :: IngressVpcConfiguration)

-- | The ID of the VPC that is used for the VPC endpoint.
ingressVpcConfiguration_vpcId :: Lens.Lens' IngressVpcConfiguration (Prelude.Maybe Prelude.Text)
ingressVpcConfiguration_vpcId = Lens.lens (\IngressVpcConfiguration' {vpcId} -> vpcId) (\s@IngressVpcConfiguration' {} a -> s {vpcId = a} :: IngressVpcConfiguration)

instance Data.FromJSON IngressVpcConfiguration where
  parseJSON =
    Data.withObject
      "IngressVpcConfiguration"
      ( \x ->
          IngressVpcConfiguration'
            Prelude.<$> (x Data..:? "VpcEndpointId")
            Prelude.<*> (x Data..:? "VpcId")
      )

instance Prelude.Hashable IngressVpcConfiguration where
  hashWithSalt _salt IngressVpcConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` vpcEndpointId
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData IngressVpcConfiguration where
  rnf IngressVpcConfiguration' {..} =
    Prelude.rnf vpcEndpointId
      `Prelude.seq` Prelude.rnf vpcId

instance Data.ToJSON IngressVpcConfiguration where
  toJSON IngressVpcConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("VpcEndpointId" Data..=) Prelude.<$> vpcEndpointId,
            ("VpcId" Data..=) Prelude.<$> vpcId
          ]
      )
