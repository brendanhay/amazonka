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
-- Module      : Amazonka.SecurityHub.Types.AwsRdsDbDomainMembership
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbDomainMembership where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an Active Directory domain membership record
-- associated with the DB instance.
--
-- /See:/ 'newAwsRdsDbDomainMembership' smart constructor.
data AwsRdsDbDomainMembership = AwsRdsDbDomainMembership'
  { -- | The status of the Active Directory Domain membership for the DB
    -- instance.
    status :: Prelude.Maybe Prelude.Text,
    -- | The fully qualified domain name of the Active Directory domain.
    fqdn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Active Directory domain.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The name of the IAM role to use when making API calls to the Directory
    -- Service.
    iamRoleName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsDbDomainMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'awsRdsDbDomainMembership_status' - The status of the Active Directory Domain membership for the DB
-- instance.
--
-- 'fqdn', 'awsRdsDbDomainMembership_fqdn' - The fully qualified domain name of the Active Directory domain.
--
-- 'domain', 'awsRdsDbDomainMembership_domain' - The identifier of the Active Directory domain.
--
-- 'iamRoleName', 'awsRdsDbDomainMembership_iamRoleName' - The name of the IAM role to use when making API calls to the Directory
-- Service.
newAwsRdsDbDomainMembership ::
  AwsRdsDbDomainMembership
newAwsRdsDbDomainMembership =
  AwsRdsDbDomainMembership'
    { status = Prelude.Nothing,
      fqdn = Prelude.Nothing,
      domain = Prelude.Nothing,
      iamRoleName = Prelude.Nothing
    }

-- | The status of the Active Directory Domain membership for the DB
-- instance.
awsRdsDbDomainMembership_status :: Lens.Lens' AwsRdsDbDomainMembership (Prelude.Maybe Prelude.Text)
awsRdsDbDomainMembership_status = Lens.lens (\AwsRdsDbDomainMembership' {status} -> status) (\s@AwsRdsDbDomainMembership' {} a -> s {status = a} :: AwsRdsDbDomainMembership)

-- | The fully qualified domain name of the Active Directory domain.
awsRdsDbDomainMembership_fqdn :: Lens.Lens' AwsRdsDbDomainMembership (Prelude.Maybe Prelude.Text)
awsRdsDbDomainMembership_fqdn = Lens.lens (\AwsRdsDbDomainMembership' {fqdn} -> fqdn) (\s@AwsRdsDbDomainMembership' {} a -> s {fqdn = a} :: AwsRdsDbDomainMembership)

-- | The identifier of the Active Directory domain.
awsRdsDbDomainMembership_domain :: Lens.Lens' AwsRdsDbDomainMembership (Prelude.Maybe Prelude.Text)
awsRdsDbDomainMembership_domain = Lens.lens (\AwsRdsDbDomainMembership' {domain} -> domain) (\s@AwsRdsDbDomainMembership' {} a -> s {domain = a} :: AwsRdsDbDomainMembership)

-- | The name of the IAM role to use when making API calls to the Directory
-- Service.
awsRdsDbDomainMembership_iamRoleName :: Lens.Lens' AwsRdsDbDomainMembership (Prelude.Maybe Prelude.Text)
awsRdsDbDomainMembership_iamRoleName = Lens.lens (\AwsRdsDbDomainMembership' {iamRoleName} -> iamRoleName) (\s@AwsRdsDbDomainMembership' {} a -> s {iamRoleName = a} :: AwsRdsDbDomainMembership)

instance Core.FromJSON AwsRdsDbDomainMembership where
  parseJSON =
    Core.withObject
      "AwsRdsDbDomainMembership"
      ( \x ->
          AwsRdsDbDomainMembership'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Fqdn")
            Prelude.<*> (x Core..:? "Domain")
            Prelude.<*> (x Core..:? "IamRoleName")
      )

instance Prelude.Hashable AwsRdsDbDomainMembership

instance Prelude.NFData AwsRdsDbDomainMembership

instance Core.ToJSON AwsRdsDbDomainMembership where
  toJSON AwsRdsDbDomainMembership' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Status" Core..=) Prelude.<$> status,
            ("Fqdn" Core..=) Prelude.<$> fqdn,
            ("Domain" Core..=) Prelude.<$> domain,
            ("IamRoleName" Core..=) Prelude.<$> iamRoleName
          ]
      )
