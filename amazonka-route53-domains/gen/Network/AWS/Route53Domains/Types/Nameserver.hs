{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    glueIps :: Prelude.Maybe [Prelude.Text],
    -- | The fully qualified host name of the name server.
    --
    -- Constraint: Maximum 255 characters
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  Nameserver
newNameserver pName_ =
  Nameserver'
    { glueIps = Prelude.Nothing,
      name = pName_
    }

-- | Glue IP address of a name server entry. Glue IP addresses are required
-- only when the name of the name server is a subdomain of the domain. For
-- example, if your domain is example.com and the name server for the
-- domain is ns.example.com, you need to specify the IP address for
-- ns.example.com.
--
-- Constraints: The list can contain only one IPv4 and one IPv6 address.
nameserver_glueIps :: Lens.Lens' Nameserver (Prelude.Maybe [Prelude.Text])
nameserver_glueIps = Lens.lens (\Nameserver' {glueIps} -> glueIps) (\s@Nameserver' {} a -> s {glueIps = a} :: Nameserver) Prelude.. Lens.mapping Prelude._Coerce

-- | The fully qualified host name of the name server.
--
-- Constraint: Maximum 255 characters
nameserver_name :: Lens.Lens' Nameserver Prelude.Text
nameserver_name = Lens.lens (\Nameserver' {name} -> name) (\s@Nameserver' {} a -> s {name = a} :: Nameserver)

instance Prelude.FromJSON Nameserver where
  parseJSON =
    Prelude.withObject
      "Nameserver"
      ( \x ->
          Nameserver'
            Prelude.<$> (x Prelude..:? "GlueIps" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..: "Name")
      )

instance Prelude.Hashable Nameserver

instance Prelude.NFData Nameserver

instance Prelude.ToJSON Nameserver where
  toJSON Nameserver' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("GlueIps" Prelude..=) Prelude.<$> glueIps,
            Prelude.Just ("Name" Prelude..= name)
          ]
      )
