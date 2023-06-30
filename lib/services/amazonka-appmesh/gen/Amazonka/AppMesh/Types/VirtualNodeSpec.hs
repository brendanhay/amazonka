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
-- Module      : Amazonka.AppMesh.Types.VirtualNodeSpec
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualNodeSpec where

import Amazonka.AppMesh.Types.Backend
import Amazonka.AppMesh.Types.BackendDefaults
import Amazonka.AppMesh.Types.Listener
import Amazonka.AppMesh.Types.Logging
import Amazonka.AppMesh.Types.ServiceDiscovery
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the specification of a virtual node.
--
-- /See:/ 'newVirtualNodeSpec' smart constructor.
data VirtualNodeSpec = VirtualNodeSpec'
  { -- | A reference to an object that represents the defaults for backends.
    backendDefaults :: Prelude.Maybe BackendDefaults,
    -- | The backends that the virtual node is expected to send outbound traffic
    -- to.
    backends :: Prelude.Maybe [Backend],
    -- | The listener that the virtual node is expected to receive inbound
    -- traffic from. You can specify one listener.
    listeners :: Prelude.Maybe [Listener],
    -- | The inbound and outbound access logging information for the virtual
    -- node.
    logging :: Prelude.Maybe Logging,
    -- | The service discovery information for the virtual node. If your virtual
    -- node does not expect ingress traffic, you can omit this parameter. If
    -- you specify a @listener@, then you must specify service discovery
    -- information.
    serviceDiscovery :: Prelude.Maybe ServiceDiscovery
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualNodeSpec' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backendDefaults', 'virtualNodeSpec_backendDefaults' - A reference to an object that represents the defaults for backends.
--
-- 'backends', 'virtualNodeSpec_backends' - The backends that the virtual node is expected to send outbound traffic
-- to.
--
-- 'listeners', 'virtualNodeSpec_listeners' - The listener that the virtual node is expected to receive inbound
-- traffic from. You can specify one listener.
--
-- 'logging', 'virtualNodeSpec_logging' - The inbound and outbound access logging information for the virtual
-- node.
--
-- 'serviceDiscovery', 'virtualNodeSpec_serviceDiscovery' - The service discovery information for the virtual node. If your virtual
-- node does not expect ingress traffic, you can omit this parameter. If
-- you specify a @listener@, then you must specify service discovery
-- information.
newVirtualNodeSpec ::
  VirtualNodeSpec
newVirtualNodeSpec =
  VirtualNodeSpec'
    { backendDefaults = Prelude.Nothing,
      backends = Prelude.Nothing,
      listeners = Prelude.Nothing,
      logging = Prelude.Nothing,
      serviceDiscovery = Prelude.Nothing
    }

-- | A reference to an object that represents the defaults for backends.
virtualNodeSpec_backendDefaults :: Lens.Lens' VirtualNodeSpec (Prelude.Maybe BackendDefaults)
virtualNodeSpec_backendDefaults = Lens.lens (\VirtualNodeSpec' {backendDefaults} -> backendDefaults) (\s@VirtualNodeSpec' {} a -> s {backendDefaults = a} :: VirtualNodeSpec)

-- | The backends that the virtual node is expected to send outbound traffic
-- to.
virtualNodeSpec_backends :: Lens.Lens' VirtualNodeSpec (Prelude.Maybe [Backend])
virtualNodeSpec_backends = Lens.lens (\VirtualNodeSpec' {backends} -> backends) (\s@VirtualNodeSpec' {} a -> s {backends = a} :: VirtualNodeSpec) Prelude.. Lens.mapping Lens.coerced

-- | The listener that the virtual node is expected to receive inbound
-- traffic from. You can specify one listener.
virtualNodeSpec_listeners :: Lens.Lens' VirtualNodeSpec (Prelude.Maybe [Listener])
virtualNodeSpec_listeners = Lens.lens (\VirtualNodeSpec' {listeners} -> listeners) (\s@VirtualNodeSpec' {} a -> s {listeners = a} :: VirtualNodeSpec) Prelude.. Lens.mapping Lens.coerced

-- | The inbound and outbound access logging information for the virtual
-- node.
virtualNodeSpec_logging :: Lens.Lens' VirtualNodeSpec (Prelude.Maybe Logging)
virtualNodeSpec_logging = Lens.lens (\VirtualNodeSpec' {logging} -> logging) (\s@VirtualNodeSpec' {} a -> s {logging = a} :: VirtualNodeSpec)

-- | The service discovery information for the virtual node. If your virtual
-- node does not expect ingress traffic, you can omit this parameter. If
-- you specify a @listener@, then you must specify service discovery
-- information.
virtualNodeSpec_serviceDiscovery :: Lens.Lens' VirtualNodeSpec (Prelude.Maybe ServiceDiscovery)
virtualNodeSpec_serviceDiscovery = Lens.lens (\VirtualNodeSpec' {serviceDiscovery} -> serviceDiscovery) (\s@VirtualNodeSpec' {} a -> s {serviceDiscovery = a} :: VirtualNodeSpec)

instance Data.FromJSON VirtualNodeSpec where
  parseJSON =
    Data.withObject
      "VirtualNodeSpec"
      ( \x ->
          VirtualNodeSpec'
            Prelude.<$> (x Data..:? "backendDefaults")
            Prelude.<*> (x Data..:? "backends" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "listeners" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "logging")
            Prelude.<*> (x Data..:? "serviceDiscovery")
      )

instance Prelude.Hashable VirtualNodeSpec where
  hashWithSalt _salt VirtualNodeSpec' {..} =
    _salt
      `Prelude.hashWithSalt` backendDefaults
      `Prelude.hashWithSalt` backends
      `Prelude.hashWithSalt` listeners
      `Prelude.hashWithSalt` logging
      `Prelude.hashWithSalt` serviceDiscovery

instance Prelude.NFData VirtualNodeSpec where
  rnf VirtualNodeSpec' {..} =
    Prelude.rnf backendDefaults
      `Prelude.seq` Prelude.rnf backends
      `Prelude.seq` Prelude.rnf listeners
      `Prelude.seq` Prelude.rnf logging
      `Prelude.seq` Prelude.rnf serviceDiscovery

instance Data.ToJSON VirtualNodeSpec where
  toJSON VirtualNodeSpec' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("backendDefaults" Data..=)
              Prelude.<$> backendDefaults,
            ("backends" Data..=) Prelude.<$> backends,
            ("listeners" Data..=) Prelude.<$> listeners,
            ("logging" Data..=) Prelude.<$> logging,
            ("serviceDiscovery" Data..=)
              Prelude.<$> serviceDiscovery
          ]
      )
