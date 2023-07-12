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
-- Module      : Amazonka.EC2.Types.LaunchTemplatePrivateDnsNameOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplatePrivateDnsNameOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.HostnameType
import qualified Amazonka.Prelude as Prelude

-- | Describes the options for instance hostnames.
--
-- /See:/ 'newLaunchTemplatePrivateDnsNameOptions' smart constructor.
data LaunchTemplatePrivateDnsNameOptions = LaunchTemplatePrivateDnsNameOptions'
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
-- Create a value of 'LaunchTemplatePrivateDnsNameOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableResourceNameDnsAAAARecord', 'launchTemplatePrivateDnsNameOptions_enableResourceNameDnsAAAARecord' - Indicates whether to respond to DNS queries for instance hostnames with
-- DNS AAAA records.
--
-- 'enableResourceNameDnsARecord', 'launchTemplatePrivateDnsNameOptions_enableResourceNameDnsARecord' - Indicates whether to respond to DNS queries for instance hostnames with
-- DNS A records.
--
-- 'hostnameType', 'launchTemplatePrivateDnsNameOptions_hostnameType' - The type of hostname to assign to an instance.
newLaunchTemplatePrivateDnsNameOptions ::
  LaunchTemplatePrivateDnsNameOptions
newLaunchTemplatePrivateDnsNameOptions =
  LaunchTemplatePrivateDnsNameOptions'
    { enableResourceNameDnsAAAARecord =
        Prelude.Nothing,
      enableResourceNameDnsARecord =
        Prelude.Nothing,
      hostnameType = Prelude.Nothing
    }

-- | Indicates whether to respond to DNS queries for instance hostnames with
-- DNS AAAA records.
launchTemplatePrivateDnsNameOptions_enableResourceNameDnsAAAARecord :: Lens.Lens' LaunchTemplatePrivateDnsNameOptions (Prelude.Maybe Prelude.Bool)
launchTemplatePrivateDnsNameOptions_enableResourceNameDnsAAAARecord = Lens.lens (\LaunchTemplatePrivateDnsNameOptions' {enableResourceNameDnsAAAARecord} -> enableResourceNameDnsAAAARecord) (\s@LaunchTemplatePrivateDnsNameOptions' {} a -> s {enableResourceNameDnsAAAARecord = a} :: LaunchTemplatePrivateDnsNameOptions)

-- | Indicates whether to respond to DNS queries for instance hostnames with
-- DNS A records.
launchTemplatePrivateDnsNameOptions_enableResourceNameDnsARecord :: Lens.Lens' LaunchTemplatePrivateDnsNameOptions (Prelude.Maybe Prelude.Bool)
launchTemplatePrivateDnsNameOptions_enableResourceNameDnsARecord = Lens.lens (\LaunchTemplatePrivateDnsNameOptions' {enableResourceNameDnsARecord} -> enableResourceNameDnsARecord) (\s@LaunchTemplatePrivateDnsNameOptions' {} a -> s {enableResourceNameDnsARecord = a} :: LaunchTemplatePrivateDnsNameOptions)

-- | The type of hostname to assign to an instance.
launchTemplatePrivateDnsNameOptions_hostnameType :: Lens.Lens' LaunchTemplatePrivateDnsNameOptions (Prelude.Maybe HostnameType)
launchTemplatePrivateDnsNameOptions_hostnameType = Lens.lens (\LaunchTemplatePrivateDnsNameOptions' {hostnameType} -> hostnameType) (\s@LaunchTemplatePrivateDnsNameOptions' {} a -> s {hostnameType = a} :: LaunchTemplatePrivateDnsNameOptions)

instance
  Data.FromXML
    LaunchTemplatePrivateDnsNameOptions
  where
  parseXML x =
    LaunchTemplatePrivateDnsNameOptions'
      Prelude.<$> (x Data..@? "enableResourceNameDnsAAAARecord")
      Prelude.<*> (x Data..@? "enableResourceNameDnsARecord")
      Prelude.<*> (x Data..@? "hostnameType")

instance
  Prelude.Hashable
    LaunchTemplatePrivateDnsNameOptions
  where
  hashWithSalt
    _salt
    LaunchTemplatePrivateDnsNameOptions' {..} =
      _salt
        `Prelude.hashWithSalt` enableResourceNameDnsAAAARecord
        `Prelude.hashWithSalt` enableResourceNameDnsARecord
        `Prelude.hashWithSalt` hostnameType

instance
  Prelude.NFData
    LaunchTemplatePrivateDnsNameOptions
  where
  rnf LaunchTemplatePrivateDnsNameOptions' {..} =
    Prelude.rnf enableResourceNameDnsAAAARecord
      `Prelude.seq` Prelude.rnf enableResourceNameDnsARecord
      `Prelude.seq` Prelude.rnf hostnameType
