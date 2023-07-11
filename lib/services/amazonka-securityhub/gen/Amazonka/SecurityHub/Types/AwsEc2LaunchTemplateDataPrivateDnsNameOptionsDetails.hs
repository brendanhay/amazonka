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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the options for Amazon EC2 instance hostnames.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails' smart constructor.
data AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails = AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails'
  { -- | Indicates whether to respond to DNS queries for instance hostnames with
    -- DNS AAAA records.
    enableResourceNameDnsAAAARecord :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether to respond to DNS queries for instance hostnames with
    -- DNS A records.
    enableResourceNameDnsARecord :: Prelude.Maybe Prelude.Bool,
    -- | The type of hostname for EC2 instances.
    hostnameType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableResourceNameDnsAAAARecord', 'awsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails_enableResourceNameDnsAAAARecord' - Indicates whether to respond to DNS queries for instance hostnames with
-- DNS AAAA records.
--
-- 'enableResourceNameDnsARecord', 'awsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails_enableResourceNameDnsARecord' - Indicates whether to respond to DNS queries for instance hostnames with
-- DNS A records.
--
-- 'hostnameType', 'awsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails_hostnameType' - The type of hostname for EC2 instances.
newAwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails ::
  AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails
newAwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails =
  AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails'
    { enableResourceNameDnsAAAARecord =
        Prelude.Nothing,
      enableResourceNameDnsARecord =
        Prelude.Nothing,
      hostnameType =
        Prelude.Nothing
    }

-- | Indicates whether to respond to DNS queries for instance hostnames with
-- DNS AAAA records.
awsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails_enableResourceNameDnsAAAARecord :: Lens.Lens' AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails (Prelude.Maybe Prelude.Bool)
awsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails_enableResourceNameDnsAAAARecord = Lens.lens (\AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails' {enableResourceNameDnsAAAARecord} -> enableResourceNameDnsAAAARecord) (\s@AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails' {} a -> s {enableResourceNameDnsAAAARecord = a} :: AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails)

-- | Indicates whether to respond to DNS queries for instance hostnames with
-- DNS A records.
awsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails_enableResourceNameDnsARecord :: Lens.Lens' AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails (Prelude.Maybe Prelude.Bool)
awsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails_enableResourceNameDnsARecord = Lens.lens (\AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails' {enableResourceNameDnsARecord} -> enableResourceNameDnsARecord) (\s@AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails' {} a -> s {enableResourceNameDnsARecord = a} :: AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails)

-- | The type of hostname for EC2 instances.
awsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails_hostnameType :: Lens.Lens' AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails_hostnameType = Lens.lens (\AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails' {hostnameType} -> hostnameType) (\s@AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails' {} a -> s {hostnameType = a} :: AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails'
            Prelude.<$> (x Data..:? "EnableResourceNameDnsAAAARecord")
            Prelude.<*> (x Data..:? "EnableResourceNameDnsARecord")
            Prelude.<*> (x Data..:? "HostnameType")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails' {..} =
      _salt
        `Prelude.hashWithSalt` enableResourceNameDnsAAAARecord
        `Prelude.hashWithSalt` enableResourceNameDnsARecord
        `Prelude.hashWithSalt` hostnameType

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails
  where
  rnf
    AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails' {..} =
      Prelude.rnf enableResourceNameDnsAAAARecord
        `Prelude.seq` Prelude.rnf enableResourceNameDnsARecord
        `Prelude.seq` Prelude.rnf hostnameType

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataPrivateDnsNameOptionsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("EnableResourceNameDnsAAAARecord" Data..=)
                Prelude.<$> enableResourceNameDnsAAAARecord,
              ("EnableResourceNameDnsARecord" Data..=)
                Prelude.<$> enableResourceNameDnsARecord,
              ("HostnameType" Data..=) Prelude.<$> hostnameType
            ]
        )
