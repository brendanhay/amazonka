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
-- Module      : Amazonka.AppMesh.Types.VirtualServiceBackend
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualServiceBackend where

import Amazonka.AppMesh.Types.ClientPolicy
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a virtual service backend for a virtual node.
--
-- /See:/ 'newVirtualServiceBackend' smart constructor.
data VirtualServiceBackend = VirtualServiceBackend'
  { -- | A reference to an object that represents the client policy for a
    -- backend.
    clientPolicy :: Prelude.Maybe ClientPolicy,
    -- | The name of the virtual service that is acting as a virtual node
    -- backend.
    virtualServiceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualServiceBackend' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientPolicy', 'virtualServiceBackend_clientPolicy' - A reference to an object that represents the client policy for a
-- backend.
--
-- 'virtualServiceName', 'virtualServiceBackend_virtualServiceName' - The name of the virtual service that is acting as a virtual node
-- backend.
newVirtualServiceBackend ::
  -- | 'virtualServiceName'
  Prelude.Text ->
  VirtualServiceBackend
newVirtualServiceBackend pVirtualServiceName_ =
  VirtualServiceBackend'
    { clientPolicy =
        Prelude.Nothing,
      virtualServiceName = pVirtualServiceName_
    }

-- | A reference to an object that represents the client policy for a
-- backend.
virtualServiceBackend_clientPolicy :: Lens.Lens' VirtualServiceBackend (Prelude.Maybe ClientPolicy)
virtualServiceBackend_clientPolicy = Lens.lens (\VirtualServiceBackend' {clientPolicy} -> clientPolicy) (\s@VirtualServiceBackend' {} a -> s {clientPolicy = a} :: VirtualServiceBackend)

-- | The name of the virtual service that is acting as a virtual node
-- backend.
virtualServiceBackend_virtualServiceName :: Lens.Lens' VirtualServiceBackend Prelude.Text
virtualServiceBackend_virtualServiceName = Lens.lens (\VirtualServiceBackend' {virtualServiceName} -> virtualServiceName) (\s@VirtualServiceBackend' {} a -> s {virtualServiceName = a} :: VirtualServiceBackend)

instance Core.FromJSON VirtualServiceBackend where
  parseJSON =
    Core.withObject
      "VirtualServiceBackend"
      ( \x ->
          VirtualServiceBackend'
            Prelude.<$> (x Core..:? "clientPolicy")
            Prelude.<*> (x Core..: "virtualServiceName")
      )

instance Prelude.Hashable VirtualServiceBackend where
  hashWithSalt _salt VirtualServiceBackend' {..} =
    _salt `Prelude.hashWithSalt` clientPolicy
      `Prelude.hashWithSalt` virtualServiceName

instance Prelude.NFData VirtualServiceBackend where
  rnf VirtualServiceBackend' {..} =
    Prelude.rnf clientPolicy
      `Prelude.seq` Prelude.rnf virtualServiceName

instance Core.ToJSON VirtualServiceBackend where
  toJSON VirtualServiceBackend' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientPolicy" Core..=) Prelude.<$> clientPolicy,
            Prelude.Just
              ("virtualServiceName" Core..= virtualServiceName)
          ]
      )
