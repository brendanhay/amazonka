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
-- Module      : Amazonka.AppMesh.Types.VirtualServiceSpec
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualServiceSpec where

import Amazonka.AppMesh.Types.VirtualServiceProvider
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the specification of a virtual service.
--
-- /See:/ 'newVirtualServiceSpec' smart constructor.
data VirtualServiceSpec = VirtualServiceSpec'
  { -- | The App Mesh object that is acting as the provider for a virtual
    -- service. You can specify a single virtual node or virtual router.
    provider :: Prelude.Maybe VirtualServiceProvider
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualServiceSpec' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provider', 'virtualServiceSpec_provider' - The App Mesh object that is acting as the provider for a virtual
-- service. You can specify a single virtual node or virtual router.
newVirtualServiceSpec ::
  VirtualServiceSpec
newVirtualServiceSpec =
  VirtualServiceSpec' {provider = Prelude.Nothing}

-- | The App Mesh object that is acting as the provider for a virtual
-- service. You can specify a single virtual node or virtual router.
virtualServiceSpec_provider :: Lens.Lens' VirtualServiceSpec (Prelude.Maybe VirtualServiceProvider)
virtualServiceSpec_provider = Lens.lens (\VirtualServiceSpec' {provider} -> provider) (\s@VirtualServiceSpec' {} a -> s {provider = a} :: VirtualServiceSpec)

instance Core.FromJSON VirtualServiceSpec where
  parseJSON =
    Core.withObject
      "VirtualServiceSpec"
      ( \x ->
          VirtualServiceSpec'
            Prelude.<$> (x Core..:? "provider")
      )

instance Prelude.Hashable VirtualServiceSpec where
  hashWithSalt _salt VirtualServiceSpec' {..} =
    _salt `Prelude.hashWithSalt` provider

instance Prelude.NFData VirtualServiceSpec where
  rnf VirtualServiceSpec' {..} = Prelude.rnf provider

instance Core.ToJSON VirtualServiceSpec where
  toJSON VirtualServiceSpec' {..} =
    Core.object
      ( Prelude.catMaybes
          [("provider" Core..=) Prelude.<$> provider]
      )
