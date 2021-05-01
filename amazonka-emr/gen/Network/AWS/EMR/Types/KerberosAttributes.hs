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
-- Module      : Network.AWS.EMR.Types.KerberosAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.KerberosAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Attributes for Kerberos configuration when Kerberos authentication is
-- enabled using a security configuration. For more information see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication>
-- in the /Amazon EMR Management Guide/.
--
-- /See:/ 'newKerberosAttributes' smart constructor.
data KerberosAttributes = KerberosAttributes'
  { -- | The name of the Kerberos realm to which all nodes in a cluster belong.
    -- For example, @EC2.INTERNAL@.
    realm :: Prelude.Maybe Prelude.Text,
    -- | Required only when establishing a cross-realm trust with an Active
    -- Directory domain. A user with sufficient privileges to join resources to
    -- the domain.
    aDDomainJoinUser :: Prelude.Maybe Prelude.Text,
    -- | The password used within the cluster for the kadmin service on the
    -- cluster-dedicated KDC, which maintains Kerberos principals, password
    -- policies, and keytabs for the cluster.
    kdcAdminPassword :: Prelude.Maybe Prelude.Text,
    -- | The Active Directory password for @ADDomainJoinUser@.
    aDDomainJoinPassword :: Prelude.Maybe Prelude.Text,
    -- | Required only when establishing a cross-realm trust with a KDC in a
    -- different realm. The cross-realm principal password, which must be
    -- identical across realms.
    crossRealmTrustPrincipalPassword :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KerberosAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'realm', 'kerberosAttributes_realm' - The name of the Kerberos realm to which all nodes in a cluster belong.
-- For example, @EC2.INTERNAL@.
--
-- 'aDDomainJoinUser', 'kerberosAttributes_aDDomainJoinUser' - Required only when establishing a cross-realm trust with an Active
-- Directory domain. A user with sufficient privileges to join resources to
-- the domain.
--
-- 'kdcAdminPassword', 'kerberosAttributes_kdcAdminPassword' - The password used within the cluster for the kadmin service on the
-- cluster-dedicated KDC, which maintains Kerberos principals, password
-- policies, and keytabs for the cluster.
--
-- 'aDDomainJoinPassword', 'kerberosAttributes_aDDomainJoinPassword' - The Active Directory password for @ADDomainJoinUser@.
--
-- 'crossRealmTrustPrincipalPassword', 'kerberosAttributes_crossRealmTrustPrincipalPassword' - Required only when establishing a cross-realm trust with a KDC in a
-- different realm. The cross-realm principal password, which must be
-- identical across realms.
newKerberosAttributes ::
  KerberosAttributes
newKerberosAttributes =
  KerberosAttributes'
    { realm = Prelude.Nothing,
      aDDomainJoinUser = Prelude.Nothing,
      kdcAdminPassword = Prelude.Nothing,
      aDDomainJoinPassword = Prelude.Nothing,
      crossRealmTrustPrincipalPassword = Prelude.Nothing
    }

-- | The name of the Kerberos realm to which all nodes in a cluster belong.
-- For example, @EC2.INTERNAL@.
kerberosAttributes_realm :: Lens.Lens' KerberosAttributes (Prelude.Maybe Prelude.Text)
kerberosAttributes_realm = Lens.lens (\KerberosAttributes' {realm} -> realm) (\s@KerberosAttributes' {} a -> s {realm = a} :: KerberosAttributes)

-- | Required only when establishing a cross-realm trust with an Active
-- Directory domain. A user with sufficient privileges to join resources to
-- the domain.
kerberosAttributes_aDDomainJoinUser :: Lens.Lens' KerberosAttributes (Prelude.Maybe Prelude.Text)
kerberosAttributes_aDDomainJoinUser = Lens.lens (\KerberosAttributes' {aDDomainJoinUser} -> aDDomainJoinUser) (\s@KerberosAttributes' {} a -> s {aDDomainJoinUser = a} :: KerberosAttributes)

-- | The password used within the cluster for the kadmin service on the
-- cluster-dedicated KDC, which maintains Kerberos principals, password
-- policies, and keytabs for the cluster.
kerberosAttributes_kdcAdminPassword :: Lens.Lens' KerberosAttributes (Prelude.Maybe Prelude.Text)
kerberosAttributes_kdcAdminPassword = Lens.lens (\KerberosAttributes' {kdcAdminPassword} -> kdcAdminPassword) (\s@KerberosAttributes' {} a -> s {kdcAdminPassword = a} :: KerberosAttributes)

-- | The Active Directory password for @ADDomainJoinUser@.
kerberosAttributes_aDDomainJoinPassword :: Lens.Lens' KerberosAttributes (Prelude.Maybe Prelude.Text)
kerberosAttributes_aDDomainJoinPassword = Lens.lens (\KerberosAttributes' {aDDomainJoinPassword} -> aDDomainJoinPassword) (\s@KerberosAttributes' {} a -> s {aDDomainJoinPassword = a} :: KerberosAttributes)

-- | Required only when establishing a cross-realm trust with a KDC in a
-- different realm. The cross-realm principal password, which must be
-- identical across realms.
kerberosAttributes_crossRealmTrustPrincipalPassword :: Lens.Lens' KerberosAttributes (Prelude.Maybe Prelude.Text)
kerberosAttributes_crossRealmTrustPrincipalPassword = Lens.lens (\KerberosAttributes' {crossRealmTrustPrincipalPassword} -> crossRealmTrustPrincipalPassword) (\s@KerberosAttributes' {} a -> s {crossRealmTrustPrincipalPassword = a} :: KerberosAttributes)

instance Prelude.FromJSON KerberosAttributes where
  parseJSON =
    Prelude.withObject
      "KerberosAttributes"
      ( \x ->
          KerberosAttributes'
            Prelude.<$> (x Prelude..:? "Realm")
            Prelude.<*> (x Prelude..:? "ADDomainJoinUser")
            Prelude.<*> (x Prelude..:? "KdcAdminPassword")
            Prelude.<*> (x Prelude..:? "ADDomainJoinPassword")
            Prelude.<*> (x Prelude..:? "CrossRealmTrustPrincipalPassword")
      )

instance Prelude.Hashable KerberosAttributes

instance Prelude.NFData KerberosAttributes

instance Prelude.ToJSON KerberosAttributes where
  toJSON KerberosAttributes' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Realm" Prelude..=) Prelude.<$> realm,
            ("ADDomainJoinUser" Prelude..=)
              Prelude.<$> aDDomainJoinUser,
            ("KdcAdminPassword" Prelude..=)
              Prelude.<$> kdcAdminPassword,
            ("ADDomainJoinPassword" Prelude..=)
              Prelude.<$> aDDomainJoinPassword,
            ("CrossRealmTrustPrincipalPassword" Prelude..=)
              Prelude.<$> crossRealmTrustPrincipalPassword
          ]
      )
