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
-- Module      : Amazonka.AppMesh.Types.VirtualServiceProvider
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualServiceProvider where

import Amazonka.AppMesh.Types.VirtualNodeServiceProvider
import Amazonka.AppMesh.Types.VirtualRouterServiceProvider
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the provider for a virtual service.
--
-- /See:/ 'newVirtualServiceProvider' smart constructor.
data VirtualServiceProvider = VirtualServiceProvider'
  { -- | The virtual router associated with a virtual service.
    virtualRouter :: Prelude.Maybe VirtualRouterServiceProvider,
    -- | The virtual node associated with a virtual service.
    virtualNode :: Prelude.Maybe VirtualNodeServiceProvider
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualServiceProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualRouter', 'virtualServiceProvider_virtualRouter' - The virtual router associated with a virtual service.
--
-- 'virtualNode', 'virtualServiceProvider_virtualNode' - The virtual node associated with a virtual service.
newVirtualServiceProvider ::
  VirtualServiceProvider
newVirtualServiceProvider =
  VirtualServiceProvider'
    { virtualRouter =
        Prelude.Nothing,
      virtualNode = Prelude.Nothing
    }

-- | The virtual router associated with a virtual service.
virtualServiceProvider_virtualRouter :: Lens.Lens' VirtualServiceProvider (Prelude.Maybe VirtualRouterServiceProvider)
virtualServiceProvider_virtualRouter = Lens.lens (\VirtualServiceProvider' {virtualRouter} -> virtualRouter) (\s@VirtualServiceProvider' {} a -> s {virtualRouter = a} :: VirtualServiceProvider)

-- | The virtual node associated with a virtual service.
virtualServiceProvider_virtualNode :: Lens.Lens' VirtualServiceProvider (Prelude.Maybe VirtualNodeServiceProvider)
virtualServiceProvider_virtualNode = Lens.lens (\VirtualServiceProvider' {virtualNode} -> virtualNode) (\s@VirtualServiceProvider' {} a -> s {virtualNode = a} :: VirtualServiceProvider)

instance Core.FromJSON VirtualServiceProvider where
  parseJSON =
    Core.withObject
      "VirtualServiceProvider"
      ( \x ->
          VirtualServiceProvider'
            Prelude.<$> (x Core..:? "virtualRouter")
            Prelude.<*> (x Core..:? "virtualNode")
      )

instance Prelude.Hashable VirtualServiceProvider where
  hashWithSalt _salt VirtualServiceProvider' {..} =
    _salt `Prelude.hashWithSalt` virtualRouter
      `Prelude.hashWithSalt` virtualNode

instance Prelude.NFData VirtualServiceProvider where
  rnf VirtualServiceProvider' {..} =
    Prelude.rnf virtualRouter
      `Prelude.seq` Prelude.rnf virtualNode

instance Core.ToJSON VirtualServiceProvider where
  toJSON VirtualServiceProvider' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("virtualRouter" Core..=) Prelude.<$> virtualRouter,
            ("virtualNode" Core..=) Prelude.<$> virtualNode
          ]
      )
