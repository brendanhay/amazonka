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
-- Module      : Amazonka.Kafka.Types.VpcConnectivity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.VpcConnectivity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.VpcConnectivityClientAuthentication
import qualified Amazonka.Prelude as Prelude

-- | VPC connectivity access control for brokers.
--
-- /See:/ 'newVpcConnectivity' smart constructor.
data VpcConnectivity = VpcConnectivity'
  { -- | Includes all client authentication information for VPC connectivity.
    clientAuthentication :: Prelude.Maybe VpcConnectivityClientAuthentication
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcConnectivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientAuthentication', 'vpcConnectivity_clientAuthentication' - Includes all client authentication information for VPC connectivity.
newVpcConnectivity ::
  VpcConnectivity
newVpcConnectivity =
  VpcConnectivity'
    { clientAuthentication =
        Prelude.Nothing
    }

-- | Includes all client authentication information for VPC connectivity.
vpcConnectivity_clientAuthentication :: Lens.Lens' VpcConnectivity (Prelude.Maybe VpcConnectivityClientAuthentication)
vpcConnectivity_clientAuthentication = Lens.lens (\VpcConnectivity' {clientAuthentication} -> clientAuthentication) (\s@VpcConnectivity' {} a -> s {clientAuthentication = a} :: VpcConnectivity)

instance Data.FromJSON VpcConnectivity where
  parseJSON =
    Data.withObject
      "VpcConnectivity"
      ( \x ->
          VpcConnectivity'
            Prelude.<$> (x Data..:? "clientAuthentication")
      )

instance Prelude.Hashable VpcConnectivity where
  hashWithSalt _salt VpcConnectivity' {..} =
    _salt `Prelude.hashWithSalt` clientAuthentication

instance Prelude.NFData VpcConnectivity where
  rnf VpcConnectivity' {..} =
    Prelude.rnf clientAuthentication

instance Data.ToJSON VpcConnectivity where
  toJSON VpcConnectivity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientAuthentication" Data..=)
              Prelude.<$> clientAuthentication
          ]
      )
