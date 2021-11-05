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
-- Module      : Amazonka.WorkMail.Types.DnsRecord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.DnsRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A DNS record uploaded to your DNS provider.
--
-- /See:/ 'newDnsRecord' smart constructor.
data DnsRecord = DnsRecord'
  { -- | The DNS hostname.- For example, @domain.example.com@.
    hostname :: Prelude.Maybe Prelude.Text,
    -- | The value returned by the DNS for a query to that hostname and record
    -- type.
    value :: Prelude.Maybe Prelude.Text,
    -- | The RFC 1035 record type. Possible values: @CNAME@, @A@, @MX@.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DnsRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostname', 'dnsRecord_hostname' - The DNS hostname.- For example, @domain.example.com@.
--
-- 'value', 'dnsRecord_value' - The value returned by the DNS for a query to that hostname and record
-- type.
--
-- 'type'', 'dnsRecord_type' - The RFC 1035 record type. Possible values: @CNAME@, @A@, @MX@.
newDnsRecord ::
  DnsRecord
newDnsRecord =
  DnsRecord'
    { hostname = Prelude.Nothing,
      value = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The DNS hostname.- For example, @domain.example.com@.
dnsRecord_hostname :: Lens.Lens' DnsRecord (Prelude.Maybe Prelude.Text)
dnsRecord_hostname = Lens.lens (\DnsRecord' {hostname} -> hostname) (\s@DnsRecord' {} a -> s {hostname = a} :: DnsRecord)

-- | The value returned by the DNS for a query to that hostname and record
-- type.
dnsRecord_value :: Lens.Lens' DnsRecord (Prelude.Maybe Prelude.Text)
dnsRecord_value = Lens.lens (\DnsRecord' {value} -> value) (\s@DnsRecord' {} a -> s {value = a} :: DnsRecord)

-- | The RFC 1035 record type. Possible values: @CNAME@, @A@, @MX@.
dnsRecord_type :: Lens.Lens' DnsRecord (Prelude.Maybe Prelude.Text)
dnsRecord_type = Lens.lens (\DnsRecord' {type'} -> type') (\s@DnsRecord' {} a -> s {type' = a} :: DnsRecord)

instance Core.FromJSON DnsRecord where
  parseJSON =
    Core.withObject
      "DnsRecord"
      ( \x ->
          DnsRecord'
            Prelude.<$> (x Core..:? "Hostname")
            Prelude.<*> (x Core..:? "Value")
            Prelude.<*> (x Core..:? "Type")
      )

instance Prelude.Hashable DnsRecord

instance Prelude.NFData DnsRecord
