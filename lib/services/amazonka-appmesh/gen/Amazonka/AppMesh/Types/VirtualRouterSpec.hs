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
-- Module      : Amazonka.AppMesh.Types.VirtualRouterSpec
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualRouterSpec where

import Amazonka.AppMesh.Types.VirtualRouterListener
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the specification of a virtual router.
--
-- /See:/ 'newVirtualRouterSpec' smart constructor.
data VirtualRouterSpec = VirtualRouterSpec'
  { -- | The listeners that the virtual router is expected to receive inbound
    -- traffic from. You can specify one listener.
    listeners :: Prelude.Maybe [VirtualRouterListener]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualRouterSpec' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listeners', 'virtualRouterSpec_listeners' - The listeners that the virtual router is expected to receive inbound
-- traffic from. You can specify one listener.
newVirtualRouterSpec ::
  VirtualRouterSpec
newVirtualRouterSpec =
  VirtualRouterSpec' {listeners = Prelude.Nothing}

-- | The listeners that the virtual router is expected to receive inbound
-- traffic from. You can specify one listener.
virtualRouterSpec_listeners :: Lens.Lens' VirtualRouterSpec (Prelude.Maybe [VirtualRouterListener])
virtualRouterSpec_listeners = Lens.lens (\VirtualRouterSpec' {listeners} -> listeners) (\s@VirtualRouterSpec' {} a -> s {listeners = a} :: VirtualRouterSpec) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON VirtualRouterSpec where
  parseJSON =
    Data.withObject
      "VirtualRouterSpec"
      ( \x ->
          VirtualRouterSpec'
            Prelude.<$> (x Data..:? "listeners" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable VirtualRouterSpec where
  hashWithSalt _salt VirtualRouterSpec' {..} =
    _salt `Prelude.hashWithSalt` listeners

instance Prelude.NFData VirtualRouterSpec where
  rnf VirtualRouterSpec' {..} = Prelude.rnf listeners

instance Data.ToJSON VirtualRouterSpec where
  toJSON VirtualRouterSpec' {..} =
    Data.object
      ( Prelude.catMaybes
          [("listeners" Data..=) Prelude.<$> listeners]
      )
