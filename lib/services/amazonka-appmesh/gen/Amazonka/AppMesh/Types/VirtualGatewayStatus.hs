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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayStatus where

import Amazonka.AppMesh.Types.VirtualGatewayStatusCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the status of the mesh resource.
--
-- /See:/ 'newVirtualGatewayStatus' smart constructor.
data VirtualGatewayStatus = VirtualGatewayStatus'
  { -- | The current status.
    status :: VirtualGatewayStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'virtualGatewayStatus_status' - The current status.
newVirtualGatewayStatus ::
  -- | 'status'
  VirtualGatewayStatusCode ->
  VirtualGatewayStatus
newVirtualGatewayStatus pStatus_ =
  VirtualGatewayStatus' {status = pStatus_}

-- | The current status.
virtualGatewayStatus_status :: Lens.Lens' VirtualGatewayStatus VirtualGatewayStatusCode
virtualGatewayStatus_status = Lens.lens (\VirtualGatewayStatus' {status} -> status) (\s@VirtualGatewayStatus' {} a -> s {status = a} :: VirtualGatewayStatus)

instance Data.FromJSON VirtualGatewayStatus where
  parseJSON =
    Data.withObject
      "VirtualGatewayStatus"
      ( \x ->
          VirtualGatewayStatus'
            Prelude.<$> (x Data..: "status")
      )

instance Prelude.Hashable VirtualGatewayStatus where
  hashWithSalt _salt VirtualGatewayStatus' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData VirtualGatewayStatus where
  rnf VirtualGatewayStatus' {..} = Prelude.rnf status
