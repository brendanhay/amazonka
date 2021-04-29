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
-- Module      : Network.AWS.RDS.Types.DomainMembership
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DomainMembership where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An Active Directory Domain membership record associated with the DB
-- instance or cluster.
--
-- /See:/ 'newDomainMembership' smart constructor.
data DomainMembership = DomainMembership'
  { -- | The status of the Active Directory Domain membership for the DB instance
    -- or cluster. Values include joined, pending-join, failed, and so on.
    status :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Active Directory Domain.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The name of the IAM role to be used when making API calls to the
    -- Directory Service.
    iAMRoleName :: Prelude.Maybe Prelude.Text,
    -- | The fully qualified domain name of the Active Directory Domain.
    fqdn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DomainMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'domainMembership_status' - The status of the Active Directory Domain membership for the DB instance
-- or cluster. Values include joined, pending-join, failed, and so on.
--
-- 'domain', 'domainMembership_domain' - The identifier of the Active Directory Domain.
--
-- 'iAMRoleName', 'domainMembership_iAMRoleName' - The name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- 'fqdn', 'domainMembership_fqdn' - The fully qualified domain name of the Active Directory Domain.
newDomainMembership ::
  DomainMembership
newDomainMembership =
  DomainMembership'
    { status = Prelude.Nothing,
      domain = Prelude.Nothing,
      iAMRoleName = Prelude.Nothing,
      fqdn = Prelude.Nothing
    }

-- | The status of the Active Directory Domain membership for the DB instance
-- or cluster. Values include joined, pending-join, failed, and so on.
domainMembership_status :: Lens.Lens' DomainMembership (Prelude.Maybe Prelude.Text)
domainMembership_status = Lens.lens (\DomainMembership' {status} -> status) (\s@DomainMembership' {} a -> s {status = a} :: DomainMembership)

-- | The identifier of the Active Directory Domain.
domainMembership_domain :: Lens.Lens' DomainMembership (Prelude.Maybe Prelude.Text)
domainMembership_domain = Lens.lens (\DomainMembership' {domain} -> domain) (\s@DomainMembership' {} a -> s {domain = a} :: DomainMembership)

-- | The name of the IAM role to be used when making API calls to the
-- Directory Service.
domainMembership_iAMRoleName :: Lens.Lens' DomainMembership (Prelude.Maybe Prelude.Text)
domainMembership_iAMRoleName = Lens.lens (\DomainMembership' {iAMRoleName} -> iAMRoleName) (\s@DomainMembership' {} a -> s {iAMRoleName = a} :: DomainMembership)

-- | The fully qualified domain name of the Active Directory Domain.
domainMembership_fqdn :: Lens.Lens' DomainMembership (Prelude.Maybe Prelude.Text)
domainMembership_fqdn = Lens.lens (\DomainMembership' {fqdn} -> fqdn) (\s@DomainMembership' {} a -> s {fqdn = a} :: DomainMembership)

instance Prelude.FromXML DomainMembership where
  parseXML x =
    DomainMembership'
      Prelude.<$> (x Prelude..@? "Status")
      Prelude.<*> (x Prelude..@? "Domain")
      Prelude.<*> (x Prelude..@? "IAMRoleName")
      Prelude.<*> (x Prelude..@? "FQDN")

instance Prelude.Hashable DomainMembership

instance Prelude.NFData DomainMembership
