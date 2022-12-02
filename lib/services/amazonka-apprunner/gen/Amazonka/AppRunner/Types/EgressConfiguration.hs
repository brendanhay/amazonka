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
-- Module      : Amazonka.AppRunner.Types.EgressConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.EgressConfiguration where

import Amazonka.AppRunner.Types.EgressType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes configuration settings related to outbound network traffic of
-- an App Runner service.
--
-- /See:/ 'newEgressConfiguration' smart constructor.
data EgressConfiguration = EgressConfiguration'
  { -- | The Amazon Resource Name (ARN) of the App Runner VPC connector that you
    -- want to associate with your App Runner service. Only valid when
    -- @EgressType = VPC@.
    vpcConnectorArn :: Prelude.Maybe Prelude.Text,
    -- | The type of egress configuration.
    --
    -- Set to @DEFAULT@ for access to resources hosted on public networks.
    --
    -- Set to @VPC@ to associate your service to a custom VPC specified by
    -- @VpcConnectorArn@.
    egressType :: Prelude.Maybe EgressType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EgressConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcConnectorArn', 'egressConfiguration_vpcConnectorArn' - The Amazon Resource Name (ARN) of the App Runner VPC connector that you
-- want to associate with your App Runner service. Only valid when
-- @EgressType = VPC@.
--
-- 'egressType', 'egressConfiguration_egressType' - The type of egress configuration.
--
-- Set to @DEFAULT@ for access to resources hosted on public networks.
--
-- Set to @VPC@ to associate your service to a custom VPC specified by
-- @VpcConnectorArn@.
newEgressConfiguration ::
  EgressConfiguration
newEgressConfiguration =
  EgressConfiguration'
    { vpcConnectorArn =
        Prelude.Nothing,
      egressType = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the App Runner VPC connector that you
-- want to associate with your App Runner service. Only valid when
-- @EgressType = VPC@.
egressConfiguration_vpcConnectorArn :: Lens.Lens' EgressConfiguration (Prelude.Maybe Prelude.Text)
egressConfiguration_vpcConnectorArn = Lens.lens (\EgressConfiguration' {vpcConnectorArn} -> vpcConnectorArn) (\s@EgressConfiguration' {} a -> s {vpcConnectorArn = a} :: EgressConfiguration)

-- | The type of egress configuration.
--
-- Set to @DEFAULT@ for access to resources hosted on public networks.
--
-- Set to @VPC@ to associate your service to a custom VPC specified by
-- @VpcConnectorArn@.
egressConfiguration_egressType :: Lens.Lens' EgressConfiguration (Prelude.Maybe EgressType)
egressConfiguration_egressType = Lens.lens (\EgressConfiguration' {egressType} -> egressType) (\s@EgressConfiguration' {} a -> s {egressType = a} :: EgressConfiguration)

instance Data.FromJSON EgressConfiguration where
  parseJSON =
    Data.withObject
      "EgressConfiguration"
      ( \x ->
          EgressConfiguration'
            Prelude.<$> (x Data..:? "VpcConnectorArn")
            Prelude.<*> (x Data..:? "EgressType")
      )

instance Prelude.Hashable EgressConfiguration where
  hashWithSalt _salt EgressConfiguration' {..} =
    _salt `Prelude.hashWithSalt` vpcConnectorArn
      `Prelude.hashWithSalt` egressType

instance Prelude.NFData EgressConfiguration where
  rnf EgressConfiguration' {..} =
    Prelude.rnf vpcConnectorArn
      `Prelude.seq` Prelude.rnf egressType

instance Data.ToJSON EgressConfiguration where
  toJSON EgressConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("VpcConnectorArn" Data..=)
              Prelude.<$> vpcConnectorArn,
            ("EgressType" Data..=) Prelude.<$> egressType
          ]
      )
