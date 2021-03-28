{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.KerberosAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.KerberosAttributes
  ( KerberosAttributes (..)
  -- * Smart constructor
  , mkKerberosAttributes
  -- * Lenses
  , kaRealm
  , kaKdcAdminPassword
  , kaADDomainJoinPassword
  , kaADDomainJoinUser
  , kaCrossRealmTrustPrincipalPassword
  ) where

import qualified Network.AWS.EMR.Types.XmlStringMaxLen256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Attributes for Kerberos configuration when Kerberos authentication is enabled using a security configuration. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication> in the /Amazon EMR Management Guide/ .
--
-- /See:/ 'mkKerberosAttributes' smart constructor.
data KerberosAttributes = KerberosAttributes'
  { realm :: Core.Maybe Types.XmlStringMaxLen256
    -- ^ The name of the Kerberos realm to which all nodes in a cluster belong. For example, @EC2.INTERNAL@ . 
  , kdcAdminPassword :: Core.Maybe Types.XmlStringMaxLen256
    -- ^ The password used within the cluster for the kadmin service on the cluster-dedicated KDC, which maintains Kerberos principals, password policies, and keytabs for the cluster.
  , aDDomainJoinPassword :: Core.Maybe Types.XmlStringMaxLen256
    -- ^ The Active Directory password for @ADDomainJoinUser@ .
  , aDDomainJoinUser :: Core.Maybe Types.XmlStringMaxLen256
    -- ^ Required only when establishing a cross-realm trust with an Active Directory domain. A user with sufficient privileges to join resources to the domain.
  , crossRealmTrustPrincipalPassword :: Core.Maybe Types.XmlStringMaxLen256
    -- ^ Required only when establishing a cross-realm trust with a KDC in a different realm. The cross-realm principal password, which must be identical across realms.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KerberosAttributes' value with any optional fields omitted.
mkKerberosAttributes
    :: KerberosAttributes
mkKerberosAttributes
  = KerberosAttributes'{realm = Core.Nothing,
                        kdcAdminPassword = Core.Nothing,
                        aDDomainJoinPassword = Core.Nothing,
                        aDDomainJoinUser = Core.Nothing,
                        crossRealmTrustPrincipalPassword = Core.Nothing}

-- | The name of the Kerberos realm to which all nodes in a cluster belong. For example, @EC2.INTERNAL@ . 
--
-- /Note:/ Consider using 'realm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaRealm :: Lens.Lens' KerberosAttributes (Core.Maybe Types.XmlStringMaxLen256)
kaRealm = Lens.field @"realm"
{-# INLINEABLE kaRealm #-}
{-# DEPRECATED realm "Use generic-lens or generic-optics with 'realm' instead"  #-}

-- | The password used within the cluster for the kadmin service on the cluster-dedicated KDC, which maintains Kerberos principals, password policies, and keytabs for the cluster.
--
-- /Note:/ Consider using 'kdcAdminPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaKdcAdminPassword :: Lens.Lens' KerberosAttributes (Core.Maybe Types.XmlStringMaxLen256)
kaKdcAdminPassword = Lens.field @"kdcAdminPassword"
{-# INLINEABLE kaKdcAdminPassword #-}
{-# DEPRECATED kdcAdminPassword "Use generic-lens or generic-optics with 'kdcAdminPassword' instead"  #-}

-- | The Active Directory password for @ADDomainJoinUser@ .
--
-- /Note:/ Consider using 'aDDomainJoinPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaADDomainJoinPassword :: Lens.Lens' KerberosAttributes (Core.Maybe Types.XmlStringMaxLen256)
kaADDomainJoinPassword = Lens.field @"aDDomainJoinPassword"
{-# INLINEABLE kaADDomainJoinPassword #-}
{-# DEPRECATED aDDomainJoinPassword "Use generic-lens or generic-optics with 'aDDomainJoinPassword' instead"  #-}

-- | Required only when establishing a cross-realm trust with an Active Directory domain. A user with sufficient privileges to join resources to the domain.
--
-- /Note:/ Consider using 'aDDomainJoinUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaADDomainJoinUser :: Lens.Lens' KerberosAttributes (Core.Maybe Types.XmlStringMaxLen256)
kaADDomainJoinUser = Lens.field @"aDDomainJoinUser"
{-# INLINEABLE kaADDomainJoinUser #-}
{-# DEPRECATED aDDomainJoinUser "Use generic-lens or generic-optics with 'aDDomainJoinUser' instead"  #-}

-- | Required only when establishing a cross-realm trust with a KDC in a different realm. The cross-realm principal password, which must be identical across realms.
--
-- /Note:/ Consider using 'crossRealmTrustPrincipalPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kaCrossRealmTrustPrincipalPassword :: Lens.Lens' KerberosAttributes (Core.Maybe Types.XmlStringMaxLen256)
kaCrossRealmTrustPrincipalPassword = Lens.field @"crossRealmTrustPrincipalPassword"
{-# INLINEABLE kaCrossRealmTrustPrincipalPassword #-}
{-# DEPRECATED crossRealmTrustPrincipalPassword "Use generic-lens or generic-optics with 'crossRealmTrustPrincipalPassword' instead"  #-}

instance Core.FromJSON KerberosAttributes where
        toJSON KerberosAttributes{..}
          = Core.object
              (Core.catMaybes
                 [("Realm" Core..=) Core.<$> realm,
                  ("KdcAdminPassword" Core..=) Core.<$> kdcAdminPassword,
                  ("ADDomainJoinPassword" Core..=) Core.<$> aDDomainJoinPassword,
                  ("ADDomainJoinUser" Core..=) Core.<$> aDDomainJoinUser,
                  ("CrossRealmTrustPrincipalPassword" Core..=) Core.<$>
                    crossRealmTrustPrincipalPassword])

instance Core.FromJSON KerberosAttributes where
        parseJSON
          = Core.withObject "KerberosAttributes" Core.$
              \ x ->
                KerberosAttributes' Core.<$>
                  (x Core..:? "Realm") Core.<*> x Core..:? "KdcAdminPassword"
                    Core.<*> x Core..:? "ADDomainJoinPassword"
                    Core.<*> x Core..:? "ADDomainJoinUser"
                    Core.<*> x Core..:? "CrossRealmTrustPrincipalPassword"
