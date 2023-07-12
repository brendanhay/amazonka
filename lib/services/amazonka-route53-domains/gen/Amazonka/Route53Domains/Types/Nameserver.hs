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
-- Module      : Amazonka.Route53Domains.Types.Nameserver
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Domains.Types.Nameserver where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Name server includes the following elements.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
nameserver_glueIps = Lens.lens (\Nameserver' {glueIps} -> glueIps) (\s@Nameserver' {} a -> s {glueIps = a} :: Nameserver) Prelude.. Lens.mapping Lens.coerced

-- | The fully qualified host name of the name server.
--
-- Constraint: Maximum 255 characters
nameserver_name :: Lens.Lens' Nameserver Prelude.Text
nameserver_name = Lens.lens (\Nameserver' {name} -> name) (\s@Nameserver' {} a -> s {name = a} :: Nameserver)

instance Data.FromJSON Nameserver where
  parseJSON =
    Data.withObject
      "Nameserver"
      ( \x ->
          Nameserver'
            Prelude.<$> (x Data..:? "GlueIps" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable Nameserver where
  hashWithSalt _salt Nameserver' {..} =
    _salt
      `Prelude.hashWithSalt` glueIps
      `Prelude.hashWithSalt` name

instance Prelude.NFData Nameserver where
  rnf Nameserver' {..} =
    Prelude.rnf glueIps `Prelude.seq` Prelude.rnf name

instance Data.ToJSON Nameserver where
  toJSON Nameserver' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GlueIps" Data..=) Prelude.<$> glueIps,
            Prelude.Just ("Name" Data..= name)
          ]
      )
