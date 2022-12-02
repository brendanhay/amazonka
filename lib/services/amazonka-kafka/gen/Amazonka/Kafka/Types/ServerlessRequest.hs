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
-- Module      : Amazonka.Kafka.Types.ServerlessRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.ServerlessRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.ServerlessClientAuthentication
import Amazonka.Kafka.Types.VpcConfig
import qualified Amazonka.Prelude as Prelude

-- | Serverless cluster request.
--
-- /See:/ 'newServerlessRequest' smart constructor.
data ServerlessRequest = ServerlessRequest'
  { -- | Includes all client authentication information.
    clientAuthentication :: Prelude.Maybe ServerlessClientAuthentication,
    -- | The configuration of the Amazon VPCs for the cluster.
    vpcConfigs :: [VpcConfig]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerlessRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientAuthentication', 'serverlessRequest_clientAuthentication' - Includes all client authentication information.
--
-- 'vpcConfigs', 'serverlessRequest_vpcConfigs' - The configuration of the Amazon VPCs for the cluster.
newServerlessRequest ::
  ServerlessRequest
newServerlessRequest =
  ServerlessRequest'
    { clientAuthentication =
        Prelude.Nothing,
      vpcConfigs = Prelude.mempty
    }

-- | Includes all client authentication information.
serverlessRequest_clientAuthentication :: Lens.Lens' ServerlessRequest (Prelude.Maybe ServerlessClientAuthentication)
serverlessRequest_clientAuthentication = Lens.lens (\ServerlessRequest' {clientAuthentication} -> clientAuthentication) (\s@ServerlessRequest' {} a -> s {clientAuthentication = a} :: ServerlessRequest)

-- | The configuration of the Amazon VPCs for the cluster.
serverlessRequest_vpcConfigs :: Lens.Lens' ServerlessRequest [VpcConfig]
serverlessRequest_vpcConfigs = Lens.lens (\ServerlessRequest' {vpcConfigs} -> vpcConfigs) (\s@ServerlessRequest' {} a -> s {vpcConfigs = a} :: ServerlessRequest) Prelude.. Lens.coerced

instance Prelude.Hashable ServerlessRequest where
  hashWithSalt _salt ServerlessRequest' {..} =
    _salt `Prelude.hashWithSalt` clientAuthentication
      `Prelude.hashWithSalt` vpcConfigs

instance Prelude.NFData ServerlessRequest where
  rnf ServerlessRequest' {..} =
    Prelude.rnf clientAuthentication
      `Prelude.seq` Prelude.rnf vpcConfigs

instance Data.ToJSON ServerlessRequest where
  toJSON ServerlessRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientAuthentication" Data..=)
              Prelude.<$> clientAuthentication,
            Prelude.Just ("vpcConfigs" Data..= vpcConfigs)
          ]
      )
