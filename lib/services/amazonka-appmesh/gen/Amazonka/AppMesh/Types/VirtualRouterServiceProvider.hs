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
-- Module      : Amazonka.AppMesh.Types.VirtualRouterServiceProvider
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualRouterServiceProvider where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a virtual node service provider.
--
-- /See:/ 'newVirtualRouterServiceProvider' smart constructor.
data VirtualRouterServiceProvider = VirtualRouterServiceProvider'
  { -- | The name of the virtual router that is acting as a service provider.
    virtualRouterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualRouterServiceProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualRouterName', 'virtualRouterServiceProvider_virtualRouterName' - The name of the virtual router that is acting as a service provider.
newVirtualRouterServiceProvider ::
  -- | 'virtualRouterName'
  Prelude.Text ->
  VirtualRouterServiceProvider
newVirtualRouterServiceProvider pVirtualRouterName_ =
  VirtualRouterServiceProvider'
    { virtualRouterName =
        pVirtualRouterName_
    }

-- | The name of the virtual router that is acting as a service provider.
virtualRouterServiceProvider_virtualRouterName :: Lens.Lens' VirtualRouterServiceProvider Prelude.Text
virtualRouterServiceProvider_virtualRouterName = Lens.lens (\VirtualRouterServiceProvider' {virtualRouterName} -> virtualRouterName) (\s@VirtualRouterServiceProvider' {} a -> s {virtualRouterName = a} :: VirtualRouterServiceProvider)

instance Data.FromJSON VirtualRouterServiceProvider where
  parseJSON =
    Data.withObject
      "VirtualRouterServiceProvider"
      ( \x ->
          VirtualRouterServiceProvider'
            Prelude.<$> (x Data..: "virtualRouterName")
      )

instance
  Prelude.Hashable
    VirtualRouterServiceProvider
  where
  hashWithSalt _salt VirtualRouterServiceProvider' {..} =
    _salt `Prelude.hashWithSalt` virtualRouterName

instance Prelude.NFData VirtualRouterServiceProvider where
  rnf VirtualRouterServiceProvider' {..} =
    Prelude.rnf virtualRouterName

instance Data.ToJSON VirtualRouterServiceProvider where
  toJSON VirtualRouterServiceProvider' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("virtualRouterName" Data..= virtualRouterName)
          ]
      )
