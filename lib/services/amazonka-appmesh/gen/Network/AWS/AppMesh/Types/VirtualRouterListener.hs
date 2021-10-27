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
-- Module      : Network.AWS.AppMesh.Types.VirtualRouterListener
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppMesh.Types.VirtualRouterListener where

import Network.AWS.AppMesh.Types.PortMapping
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that represents a virtual router listener.
--
-- /See:/ 'newVirtualRouterListener' smart constructor.
data VirtualRouterListener = VirtualRouterListener'
  { portMapping :: PortMapping
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualRouterListener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portMapping', 'virtualRouterListener_portMapping' - Undocumented member.
newVirtualRouterListener ::
  -- | 'portMapping'
  PortMapping ->
  VirtualRouterListener
newVirtualRouterListener pPortMapping_ =
  VirtualRouterListener' {portMapping = pPortMapping_}

-- | Undocumented member.
virtualRouterListener_portMapping :: Lens.Lens' VirtualRouterListener PortMapping
virtualRouterListener_portMapping = Lens.lens (\VirtualRouterListener' {portMapping} -> portMapping) (\s@VirtualRouterListener' {} a -> s {portMapping = a} :: VirtualRouterListener)

instance Core.FromJSON VirtualRouterListener where
  parseJSON =
    Core.withObject
      "VirtualRouterListener"
      ( \x ->
          VirtualRouterListener'
            Prelude.<$> (x Core..: "portMapping")
      )

instance Prelude.Hashable VirtualRouterListener

instance Prelude.NFData VirtualRouterListener

instance Core.ToJSON VirtualRouterListener where
  toJSON VirtualRouterListener' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("portMapping" Core..= portMapping)]
      )
