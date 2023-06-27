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
-- Module      : Amazonka.AppRunner.Types.VpcDNSTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.VpcDNSTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | DNS Target record for a custom domain of this Amazon VPC.
--
-- /See:/ 'newVpcDNSTarget' smart constructor.
data VpcDNSTarget = VpcDNSTarget'
  { -- | The domain name of your target DNS that is associated with the Amazon
    -- VPC.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon VPC that is associated with the custom domain name
    -- of the target DNS.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the VPC Ingress Connection that is
    -- associated with your service.
    vpcIngressConnectionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcDNSTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'vpcDNSTarget_domainName' - The domain name of your target DNS that is associated with the Amazon
-- VPC.
--
-- 'vpcId', 'vpcDNSTarget_vpcId' - The ID of the Amazon VPC that is associated with the custom domain name
-- of the target DNS.
--
-- 'vpcIngressConnectionArn', 'vpcDNSTarget_vpcIngressConnectionArn' - The Amazon Resource Name (ARN) of the VPC Ingress Connection that is
-- associated with your service.
newVpcDNSTarget ::
  VpcDNSTarget
newVpcDNSTarget =
  VpcDNSTarget'
    { domainName = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      vpcIngressConnectionArn = Prelude.Nothing
    }

-- | The domain name of your target DNS that is associated with the Amazon
-- VPC.
vpcDNSTarget_domainName :: Lens.Lens' VpcDNSTarget (Prelude.Maybe Prelude.Text)
vpcDNSTarget_domainName = Lens.lens (\VpcDNSTarget' {domainName} -> domainName) (\s@VpcDNSTarget' {} a -> s {domainName = a} :: VpcDNSTarget)

-- | The ID of the Amazon VPC that is associated with the custom domain name
-- of the target DNS.
vpcDNSTarget_vpcId :: Lens.Lens' VpcDNSTarget (Prelude.Maybe Prelude.Text)
vpcDNSTarget_vpcId = Lens.lens (\VpcDNSTarget' {vpcId} -> vpcId) (\s@VpcDNSTarget' {} a -> s {vpcId = a} :: VpcDNSTarget)

-- | The Amazon Resource Name (ARN) of the VPC Ingress Connection that is
-- associated with your service.
vpcDNSTarget_vpcIngressConnectionArn :: Lens.Lens' VpcDNSTarget (Prelude.Maybe Prelude.Text)
vpcDNSTarget_vpcIngressConnectionArn = Lens.lens (\VpcDNSTarget' {vpcIngressConnectionArn} -> vpcIngressConnectionArn) (\s@VpcDNSTarget' {} a -> s {vpcIngressConnectionArn = a} :: VpcDNSTarget)

instance Data.FromJSON VpcDNSTarget where
  parseJSON =
    Data.withObject
      "VpcDNSTarget"
      ( \x ->
          VpcDNSTarget'
            Prelude.<$> (x Data..:? "DomainName")
            Prelude.<*> (x Data..:? "VpcId")
            Prelude.<*> (x Data..:? "VpcIngressConnectionArn")
      )

instance Prelude.Hashable VpcDNSTarget where
  hashWithSalt _salt VpcDNSTarget' {..} =
    _salt
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` vpcIngressConnectionArn

instance Prelude.NFData VpcDNSTarget where
  rnf VpcDNSTarget' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf vpcIngressConnectionArn
