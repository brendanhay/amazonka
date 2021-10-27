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
-- Module      : Network.AWS.AppMesh.Types.VirtualNodeStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppMesh.Types.VirtualNodeStatus where

import Network.AWS.AppMesh.Types.VirtualNodeStatusCode
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON VirtualNodeStatus where
  parseJSON =
    Core.withObject
      "VirtualNodeStatus"
      ( \x ->
          VirtualNodeStatus' Prelude.<$> (x Core..: "status")
      )

instance Prelude.Hashable VirtualNodeStatus

instance Prelude.NFData VirtualNodeStatus
