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
-- Module      : Amazonka.Kafka.Types.Serverless
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.Serverless where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.ServerlessClientAuthentication
import Amazonka.Kafka.Types.VpcConfig
import qualified Amazonka.Prelude as Prelude

-- | Serverless cluster.
--
-- /See:/ 'newServerless' smart constructor.
data Serverless = Serverless'
  { -- | Includes all client authentication information.
    clientAuthentication :: Prelude.Maybe ServerlessClientAuthentication,
    -- | The configuration of the Amazon VPCs for the cluster.
    vpcConfigs :: [VpcConfig]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Serverless' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientAuthentication', 'serverless_clientAuthentication' - Includes all client authentication information.
--
-- 'vpcConfigs', 'serverless_vpcConfigs' - The configuration of the Amazon VPCs for the cluster.
newServerless ::
  Serverless
newServerless =
  Serverless'
    { clientAuthentication = Prelude.Nothing,
      vpcConfigs = Prelude.mempty
    }

-- | Includes all client authentication information.
serverless_clientAuthentication :: Lens.Lens' Serverless (Prelude.Maybe ServerlessClientAuthentication)
serverless_clientAuthentication = Lens.lens (\Serverless' {clientAuthentication} -> clientAuthentication) (\s@Serverless' {} a -> s {clientAuthentication = a} :: Serverless)

-- | The configuration of the Amazon VPCs for the cluster.
serverless_vpcConfigs :: Lens.Lens' Serverless [VpcConfig]
serverless_vpcConfigs = Lens.lens (\Serverless' {vpcConfigs} -> vpcConfigs) (\s@Serverless' {} a -> s {vpcConfigs = a} :: Serverless) Prelude.. Lens.coerced

instance Data.FromJSON Serverless where
  parseJSON =
    Data.withObject
      "Serverless"
      ( \x ->
          Serverless'
            Prelude.<$> (x Data..:? "clientAuthentication")
            Prelude.<*> (x Data..:? "vpcConfigs" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Serverless where
  hashWithSalt _salt Serverless' {..} =
    _salt
      `Prelude.hashWithSalt` clientAuthentication
      `Prelude.hashWithSalt` vpcConfigs

instance Prelude.NFData Serverless where
  rnf Serverless' {..} =
    Prelude.rnf clientAuthentication `Prelude.seq`
      Prelude.rnf vpcConfigs
