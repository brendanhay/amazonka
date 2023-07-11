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
-- Module      : Amazonka.AppMesh.Types.VirtualNodeStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualNodeStatus where

import Amazonka.AppMesh.Types.VirtualNodeStatusCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the current status of the virtual node.
--
-- /See:/ 'newVirtualNodeStatus' smart constructor.
data VirtualNodeStatus = VirtualNodeStatus'
  { -- | The current status of the virtual node.
    status :: VirtualNodeStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualNodeStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'virtualNodeStatus_status' - The current status of the virtual node.
newVirtualNodeStatus ::
  -- | 'status'
  VirtualNodeStatusCode ->
  VirtualNodeStatus
newVirtualNodeStatus pStatus_ =
  VirtualNodeStatus' {status = pStatus_}

-- | The current status of the virtual node.
virtualNodeStatus_status :: Lens.Lens' VirtualNodeStatus VirtualNodeStatusCode
virtualNodeStatus_status = Lens.lens (\VirtualNodeStatus' {status} -> status) (\s@VirtualNodeStatus' {} a -> s {status = a} :: VirtualNodeStatus)

instance Data.FromJSON VirtualNodeStatus where
  parseJSON =
    Data.withObject
      "VirtualNodeStatus"
      ( \x ->
          VirtualNodeStatus' Prelude.<$> (x Data..: "status")
      )

instance Prelude.Hashable VirtualNodeStatus where
  hashWithSalt _salt VirtualNodeStatus' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData VirtualNodeStatus where
  rnf VirtualNodeStatus' {..} = Prelude.rnf status
