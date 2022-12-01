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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayBackendDefaults
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayBackendDefaults where

import Amazonka.AppMesh.Types.VirtualGatewayClientPolicy
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the default properties for a backend.
--
-- /See:/ 'newVirtualGatewayBackendDefaults' smart constructor.
data VirtualGatewayBackendDefaults = VirtualGatewayBackendDefaults'
  { -- | A reference to an object that represents a client policy.
    clientPolicy :: Prelude.Maybe VirtualGatewayClientPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayBackendDefaults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientPolicy', 'virtualGatewayBackendDefaults_clientPolicy' - A reference to an object that represents a client policy.
newVirtualGatewayBackendDefaults ::
  VirtualGatewayBackendDefaults
newVirtualGatewayBackendDefaults =
  VirtualGatewayBackendDefaults'
    { clientPolicy =
        Prelude.Nothing
    }

-- | A reference to an object that represents a client policy.
virtualGatewayBackendDefaults_clientPolicy :: Lens.Lens' VirtualGatewayBackendDefaults (Prelude.Maybe VirtualGatewayClientPolicy)
virtualGatewayBackendDefaults_clientPolicy = Lens.lens (\VirtualGatewayBackendDefaults' {clientPolicy} -> clientPolicy) (\s@VirtualGatewayBackendDefaults' {} a -> s {clientPolicy = a} :: VirtualGatewayBackendDefaults)

instance Core.FromJSON VirtualGatewayBackendDefaults where
  parseJSON =
    Core.withObject
      "VirtualGatewayBackendDefaults"
      ( \x ->
          VirtualGatewayBackendDefaults'
            Prelude.<$> (x Core..:? "clientPolicy")
      )

instance
  Prelude.Hashable
    VirtualGatewayBackendDefaults
  where
  hashWithSalt _salt VirtualGatewayBackendDefaults' {..} =
    _salt `Prelude.hashWithSalt` clientPolicy

instance Prelude.NFData VirtualGatewayBackendDefaults where
  rnf VirtualGatewayBackendDefaults' {..} =
    Prelude.rnf clientPolicy

instance Core.ToJSON VirtualGatewayBackendDefaults where
  toJSON VirtualGatewayBackendDefaults' {..} =
    Core.object
      ( Prelude.catMaybes
          [("clientPolicy" Core..=) Prelude.<$> clientPolicy]
      )
