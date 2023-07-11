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
-- Module      : Amazonka.EC2.Types.PrivateDnsNameOptionsResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.PrivateDnsNameOptionsResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.HostnameType
import qualified Amazonka.Prelude as Prelude

-- | Describes the options for instance hostnames.
--
-- /See:/ 'newPrivateDnsNameOptionsResponse' smart constructor.
data PrivateDnsNameOptionsResponse = PrivateDnsNameOptionsResponse'
  { -- | Indicates whether to respond to DNS queries for instance hostnames with
    -- DNS AAAA records.
    enableResourceNameDnsAAAARecord :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether to respond to DNS queries for instance hostnames with
    -- DNS A records.
    enableResourceNameDnsARecord :: Prelude.Maybe Prelude.Bool,
    -- | The type of hostname to assign to an instance.
    hostnameType :: Prelude.Maybe HostnameType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PrivateDnsNameOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableResourceNameDnsAAAARecord', 'privateDnsNameOptionsResponse_enableResourceNameDnsAAAARecord' - Indicates whether to respond to DNS queries for instance hostnames with
-- DNS AAAA records.
--
-- 'enableResourceNameDnsARecord', 'privateDnsNameOptionsResponse_enableResourceNameDnsARecord' - Indicates whether to respond to DNS queries for instance hostnames with
-- DNS A records.
--
-- 'hostnameType', 'privateDnsNameOptionsResponse_hostnameType' - The type of hostname to assign to an instance.
newPrivateDnsNameOptionsResponse ::
  PrivateDnsNameOptionsResponse
newPrivateDnsNameOptionsResponse =
  PrivateDnsNameOptionsResponse'
    { enableResourceNameDnsAAAARecord =
        Prelude.Nothing,
      enableResourceNameDnsARecord =
        Prelude.Nothing,
      hostnameType = Prelude.Nothing
    }

-- | Indicates whether to respond to DNS queries for instance hostnames with
-- DNS AAAA records.
privateDnsNameOptionsResponse_enableResourceNameDnsAAAARecord :: Lens.Lens' PrivateDnsNameOptionsResponse (Prelude.Maybe Prelude.Bool)
privateDnsNameOptionsResponse_enableResourceNameDnsAAAARecord = Lens.lens (\PrivateDnsNameOptionsResponse' {enableResourceNameDnsAAAARecord} -> enableResourceNameDnsAAAARecord) (\s@PrivateDnsNameOptionsResponse' {} a -> s {enableResourceNameDnsAAAARecord = a} :: PrivateDnsNameOptionsResponse)

-- | Indicates whether to respond to DNS queries for instance hostnames with
-- DNS A records.
privateDnsNameOptionsResponse_enableResourceNameDnsARecord :: Lens.Lens' PrivateDnsNameOptionsResponse (Prelude.Maybe Prelude.Bool)
privateDnsNameOptionsResponse_enableResourceNameDnsARecord = Lens.lens (\PrivateDnsNameOptionsResponse' {enableResourceNameDnsARecord} -> enableResourceNameDnsARecord) (\s@PrivateDnsNameOptionsResponse' {} a -> s {enableResourceNameDnsARecord = a} :: PrivateDnsNameOptionsResponse)

-- | The type of hostname to assign to an instance.
privateDnsNameOptionsResponse_hostnameType :: Lens.Lens' PrivateDnsNameOptionsResponse (Prelude.Maybe HostnameType)
privateDnsNameOptionsResponse_hostnameType = Lens.lens (\PrivateDnsNameOptionsResponse' {hostnameType} -> hostnameType) (\s@PrivateDnsNameOptionsResponse' {} a -> s {hostnameType = a} :: PrivateDnsNameOptionsResponse)

instance Data.FromXML PrivateDnsNameOptionsResponse where
  parseXML x =
    PrivateDnsNameOptionsResponse'
      Prelude.<$> (x Data..@? "enableResourceNameDnsAAAARecord")
      Prelude.<*> (x Data..@? "enableResourceNameDnsARecord")
      Prelude.<*> (x Data..@? "hostnameType")

instance
  Prelude.Hashable
    PrivateDnsNameOptionsResponse
  where
  hashWithSalt _salt PrivateDnsNameOptionsResponse' {..} =
    _salt
      `Prelude.hashWithSalt` enableResourceNameDnsAAAARecord
      `Prelude.hashWithSalt` enableResourceNameDnsARecord
      `Prelude.hashWithSalt` hostnameType

instance Prelude.NFData PrivateDnsNameOptionsResponse where
  rnf PrivateDnsNameOptionsResponse' {..} =
    Prelude.rnf enableResourceNameDnsAAAARecord
      `Prelude.seq` Prelude.rnf enableResourceNameDnsARecord
      `Prelude.seq` Prelude.rnf hostnameType
