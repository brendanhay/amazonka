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
-- Module      : Network.AWS.Route53Domains.Types.Nameserver
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.Nameserver where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Nameserver includes the following elements.
--
-- /See:/ 'newNameserver' smart constructor.
data Nameserver = Nameserver'
  { -- | Glue IP address of a name server entry. Glue IP addresses are required
    -- only when the name of the name server is a subdomain of the domain. For
    -- example, if your domain is example.com and the name server for the
    -- domain is ns.example.com, you need to specify the IP address for
    -- ns.example.com.
    --
    -- Constraints: The list can contain only one IPv4 and one IPv6 address.
    glueIps :: Core.Maybe [Core.Text],
    -- | The fully qualified host name of the name server.
    --
    -- Constraint: Maximum 255 characters
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Nameserver' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'glueIps', 'nameserver_glueIps' - Glue IP address of a name server entry. Glue IP addresses are required
-- only when the name of the name server is a subdomain of the domain. For
-- example, if your domain is example.com and the name server for the
-- domain is ns.example.com, you need to specify the IP address for
-- ns.example.com.
--
-- Constraints: The list can contain only one IPv4 and one IPv6 address.
--
-- 'name', 'nameserver_name' - The fully qualified host name of the name server.
--
-- Constraint: Maximum 255 characters
newNameserver ::
  -- | 'name'
  Core.Text ->
  Nameserver
newNameserver pName_ =
  Nameserver' {glueIps = Core.Nothing, name = pName_}

-- | Glue IP address of a name server entry. Glue IP addresses are required
-- only when the name of the name server is a subdomain of the domain. For
-- example, if your domain is example.com and the name server for the
-- domain is ns.example.com, you need to specify the IP address for
-- ns.example.com.
--
-- Constraints: The list can contain only one IPv4 and one IPv6 address.
nameserver_glueIps :: Lens.Lens' Nameserver (Core.Maybe [Core.Text])
nameserver_glueIps = Lens.lens (\Nameserver' {glueIps} -> glueIps) (\s@Nameserver' {} a -> s {glueIps = a} :: Nameserver) Core.. Lens.mapping Lens._Coerce

-- | The fully qualified host name of the name server.
--
-- Constraint: Maximum 255 characters
nameserver_name :: Lens.Lens' Nameserver Core.Text
nameserver_name = Lens.lens (\Nameserver' {name} -> name) (\s@Nameserver' {} a -> s {name = a} :: Nameserver)

instance Core.FromJSON Nameserver where
  parseJSON =
    Core.withObject
      "Nameserver"
      ( \x ->
          Nameserver'
            Core.<$> (x Core..:? "GlueIps" Core..!= Core.mempty)
            Core.<*> (x Core..: "Name")
      )

instance Core.Hashable Nameserver

instance Core.NFData Nameserver

instance Core.ToJSON Nameserver where
  toJSON Nameserver' {..} =
    Core.object
      ( Core.catMaybes
          [ ("GlueIps" Core..=) Core.<$> glueIps,
            Core.Just ("Name" Core..= name)
          ]
      )
