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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | The type of egress configuration.
    --
    -- Set to @DEFAULT@ for access to resources hosted on public networks.
    --
    -- Set to @VPC@ to associate your service to a custom VPC specified by
    -- @VpcConnectorArn@.
    egressType :: Prelude.Maybe EgressType,
    -- | The Amazon Resource Name (ARN) of the App Runner VPC connector that you
    -- want to associate with your App Runner service. Only valid when
    -- @EgressType = VPC@.
    vpcConnectorArn :: Prelude.Maybe Prelude.Text
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
-- 'egressType', 'egressConfiguration_egressType' - The type of egress configuration.
--
-- Set to @DEFAULT@ for access to resources hosted on public networks.
--
-- Set to @VPC@ to associate your service to a custom VPC specified by
-- @VpcConnectorArn@.
--
-- 'vpcConnectorArn', 'egressConfiguration_vpcConnectorArn' - The Amazon Resource Name (ARN) of the App Runner VPC connector that you
-- want to associate with your App Runner service. Only valid when
-- @EgressType = VPC@.
newEgressConfiguration ::
  EgressConfiguration
newEgressConfiguration =
  EgressConfiguration'
    { egressType = Prelude.Nothing,
      vpcConnectorArn = Prelude.Nothing
    }

-- | The type of egress configuration.
--
-- Set to @DEFAULT@ for access to resources hosted on public networks.
--
-- Set to @VPC@ to associate your service to a custom VPC specified by
-- @VpcConnectorArn@.
egressConfiguration_egressType :: Lens.Lens' EgressConfiguration (Prelude.Maybe EgressType)
egressConfiguration_egressType = Lens.lens (\EgressConfiguration' {egressType} -> egressType) (\s@EgressConfiguration' {} a -> s {egressType = a} :: EgressConfiguration)

-- | The Amazon Resource Name (ARN) of the App Runner VPC connector that you
-- want to associate with your App Runner service. Only valid when
-- @EgressType = VPC@.
egressConfiguration_vpcConnectorArn :: Lens.Lens' EgressConfiguration (Prelude.Maybe Prelude.Text)
egressConfiguration_vpcConnectorArn = Lens.lens (\EgressConfiguration' {vpcConnectorArn} -> vpcConnectorArn) (\s@EgressConfiguration' {} a -> s {vpcConnectorArn = a} :: EgressConfiguration)

instance Data.FromJSON EgressConfiguration where
  parseJSON =
    Data.withObject
      "EgressConfiguration"
      ( \x ->
          EgressConfiguration'
            Prelude.<$> (x Data..:? "EgressType")
            Prelude.<*> (x Data..:? "VpcConnectorArn")
      )

instance Prelude.Hashable EgressConfiguration where
  hashWithSalt _salt EgressConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` egressType
      `Prelude.hashWithSalt` vpcConnectorArn

instance Prelude.NFData EgressConfiguration where
  rnf EgressConfiguration' {..} =
    Prelude.rnf egressType
      `Prelude.seq` Prelude.rnf vpcConnectorArn

instance Data.ToJSON EgressConfiguration where
  toJSON EgressConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EgressType" Data..=) Prelude.<$> egressType,
            ("VpcConnectorArn" Data..=)
              Prelude.<$> vpcConnectorArn
          ]
      )
