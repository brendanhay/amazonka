-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.KerberosAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.KerberosAttributes
  ( KerberosAttributes (..),

    -- * Smart constructor
    mkKerberosAttributes,

    -- * Lenses
    kaKdcAdminPassword,
    kaRealm,
    kaADDomainJoinPassword,
    kaCrossRealmTrustPrincipalPassword,
    kaADDomainJoinUser,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Attributes for Kerberos configuration when Kerberos authentication is enabled using a security configuration. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication> in the /Amazon EMR Management Guide/ .
--
-- /See:/ 'mkKerberosAttributes' smart constructor.
data KerberosAttributes = KerberosAttributes'
  { kdcAdminPassword ::
      Lude.Maybe Lude.Text,
    realm :: Lude.Maybe Lude.Text,
    aDDomainJoinPassword :: Lude.Maybe Lude.Text,
    crossRealmTrustPrincipalPassword ::
      Lude.Maybe Lude.Text,
    aDDomainJoinUser :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KerberosAttributes' with the minimum fields required to make a request.
--
-- * 'aDDomainJoinPassword' - The Active Directory password for @ADDomainJoinUser@ .
-- * 'aDDomainJoinUser' - Required only when establishing a cross-realm trust with an Active Directory domain. A user with sufficient privileges to join resources to the domain.
-- * 'crossRealmTrustPrincipalPassword' - Required only when establishing a cross-realm trust with a KDC in a different realm. The cross-realm principal password, which must be identical across realms.
-- * 'kdcAdminPassword' - The password used within the cluster for the kadmin service on the cluster-dedicated KDC, which maintains Kerberos principals, password policies, and keytabs for the cluster.
-- * 'realm' - The name of the Kerberos realm to which all nodes in a cluster belong. For example, @EC2.INTERNAL@ .
mkKerberosAttributes ::
  KerberosAttributes
mkKerberosAttributes =
  KerberosAttributes'
    { kdcAdminPassword = Lude.Nothing,
      realm = Lude.Nothing,
      aDDomainJoinPassword = Lude.Nothing,
      crossRealmTrustPrincipalPassword = Lude.Nothing,
      aDDomainJoinUser = Lude.Nothing
    }

-- | The password used within the cluster for the kadmin service on the cluster-dedicated KDC, which maintains Kerberos principals, password policies, and keytabs for the cluster.
--
-- /Note:/ Consider using 'kdcAdminPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaKdcAdminPassword :: Lens.Lens' KerberosAttributes (Lude.Maybe Lude.Text)
kaKdcAdminPassword = Lens.lens (kdcAdminPassword :: KerberosAttributes -> Lude.Maybe Lude.Text) (\s a -> s {kdcAdminPassword = a} :: KerberosAttributes)
{-# DEPRECATED kaKdcAdminPassword "Use generic-lens or generic-optics with 'kdcAdminPassword' instead." #-}

-- | The name of the Kerberos realm to which all nodes in a cluster belong. For example, @EC2.INTERNAL@ .
--
-- /Note:/ Consider using 'realm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaRealm :: Lens.Lens' KerberosAttributes (Lude.Maybe Lude.Text)
kaRealm = Lens.lens (realm :: KerberosAttributes -> Lude.Maybe Lude.Text) (\s a -> s {realm = a} :: KerberosAttributes)
{-# DEPRECATED kaRealm "Use generic-lens or generic-optics with 'realm' instead." #-}

-- | The Active Directory password for @ADDomainJoinUser@ .
--
-- /Note:/ Consider using 'aDDomainJoinPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaADDomainJoinPassword :: Lens.Lens' KerberosAttributes (Lude.Maybe Lude.Text)
kaADDomainJoinPassword = Lens.lens (aDDomainJoinPassword :: KerberosAttributes -> Lude.Maybe Lude.Text) (\s a -> s {aDDomainJoinPassword = a} :: KerberosAttributes)
{-# DEPRECATED kaADDomainJoinPassword "Use generic-lens or generic-optics with 'aDDomainJoinPassword' instead." #-}

-- | Required only when establishing a cross-realm trust with a KDC in a different realm. The cross-realm principal password, which must be identical across realms.
--
-- /Note:/ Consider using 'crossRealmTrustPrincipalPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaCrossRealmTrustPrincipalPassword :: Lens.Lens' KerberosAttributes (Lude.Maybe Lude.Text)
kaCrossRealmTrustPrincipalPassword = Lens.lens (crossRealmTrustPrincipalPassword :: KerberosAttributes -> Lude.Maybe Lude.Text) (\s a -> s {crossRealmTrustPrincipalPassword = a} :: KerberosAttributes)
{-# DEPRECATED kaCrossRealmTrustPrincipalPassword "Use generic-lens or generic-optics with 'crossRealmTrustPrincipalPassword' instead." #-}

-- | Required only when establishing a cross-realm trust with an Active Directory domain. A user with sufficient privileges to join resources to the domain.
--
-- /Note:/ Consider using 'aDDomainJoinUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaADDomainJoinUser :: Lens.Lens' KerberosAttributes (Lude.Maybe Lude.Text)
kaADDomainJoinUser = Lens.lens (aDDomainJoinUser :: KerberosAttributes -> Lude.Maybe Lude.Text) (\s a -> s {aDDomainJoinUser = a} :: KerberosAttributes)
{-# DEPRECATED kaADDomainJoinUser "Use generic-lens or generic-optics with 'aDDomainJoinUser' instead." #-}

instance Lude.FromJSON KerberosAttributes where
  parseJSON =
    Lude.withObject
      "KerberosAttributes"
      ( \x ->
          KerberosAttributes'
            Lude.<$> (x Lude..:? "KdcAdminPassword")
            Lude.<*> (x Lude..:? "Realm")
            Lude.<*> (x Lude..:? "ADDomainJoinPassword")
            Lude.<*> (x Lude..:? "CrossRealmTrustPrincipalPassword")
            Lude.<*> (x Lude..:? "ADDomainJoinUser")
      )

instance Lude.ToJSON KerberosAttributes where
  toJSON KerberosAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KdcAdminPassword" Lude..=) Lude.<$> kdcAdminPassword,
            ("Realm" Lude..=) Lude.<$> realm,
            ("ADDomainJoinPassword" Lude..=) Lude.<$> aDDomainJoinPassword,
            ("CrossRealmTrustPrincipalPassword" Lude..=)
              Lude.<$> crossRealmTrustPrincipalPassword,
            ("ADDomainJoinUser" Lude..=) Lude.<$> aDDomainJoinUser
          ]
      )
