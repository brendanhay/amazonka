{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.KerberosAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.KerberosAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Attributes for Kerberos configuration when Kerberos authentication is enabled using a security configuration. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication> in the /Amazon EMR Management Guide/ .
--
--
--
-- /See:/ 'kerberosAttributes' smart constructor.
data KerberosAttributes = KerberosAttributes'
  { _kaKdcAdminPassword ::
      !(Maybe Text),
    _kaRealm :: !(Maybe Text),
    _kaADDomainJoinPassword :: !(Maybe Text),
    _kaCrossRealmTrustPrincipalPassword :: !(Maybe Text),
    _kaADDomainJoinUser :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KerberosAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kaKdcAdminPassword' - The password used within the cluster for the kadmin service on the cluster-dedicated KDC, which maintains Kerberos principals, password policies, and keytabs for the cluster.
--
-- * 'kaRealm' - The name of the Kerberos realm to which all nodes in a cluster belong. For example, @EC2.INTERNAL@ .
--
-- * 'kaADDomainJoinPassword' - The Active Directory password for @ADDomainJoinUser@ .
--
-- * 'kaCrossRealmTrustPrincipalPassword' - Required only when establishing a cross-realm trust with a KDC in a different realm. The cross-realm principal password, which must be identical across realms.
--
-- * 'kaADDomainJoinUser' - Required only when establishing a cross-realm trust with an Active Directory domain. A user with sufficient privileges to join resources to the domain.
kerberosAttributes ::
  KerberosAttributes
kerberosAttributes =
  KerberosAttributes'
    { _kaKdcAdminPassword = Nothing,
      _kaRealm = Nothing,
      _kaADDomainJoinPassword = Nothing,
      _kaCrossRealmTrustPrincipalPassword = Nothing,
      _kaADDomainJoinUser = Nothing
    }

-- | The password used within the cluster for the kadmin service on the cluster-dedicated KDC, which maintains Kerberos principals, password policies, and keytabs for the cluster.
kaKdcAdminPassword :: Lens' KerberosAttributes (Maybe Text)
kaKdcAdminPassword = lens _kaKdcAdminPassword (\s a -> s {_kaKdcAdminPassword = a})

-- | The name of the Kerberos realm to which all nodes in a cluster belong. For example, @EC2.INTERNAL@ .
kaRealm :: Lens' KerberosAttributes (Maybe Text)
kaRealm = lens _kaRealm (\s a -> s {_kaRealm = a})

-- | The Active Directory password for @ADDomainJoinUser@ .
kaADDomainJoinPassword :: Lens' KerberosAttributes (Maybe Text)
kaADDomainJoinPassword = lens _kaADDomainJoinPassword (\s a -> s {_kaADDomainJoinPassword = a})

-- | Required only when establishing a cross-realm trust with a KDC in a different realm. The cross-realm principal password, which must be identical across realms.
kaCrossRealmTrustPrincipalPassword :: Lens' KerberosAttributes (Maybe Text)
kaCrossRealmTrustPrincipalPassword = lens _kaCrossRealmTrustPrincipalPassword (\s a -> s {_kaCrossRealmTrustPrincipalPassword = a})

-- | Required only when establishing a cross-realm trust with an Active Directory domain. A user with sufficient privileges to join resources to the domain.
kaADDomainJoinUser :: Lens' KerberosAttributes (Maybe Text)
kaADDomainJoinUser = lens _kaADDomainJoinUser (\s a -> s {_kaADDomainJoinUser = a})

instance FromJSON KerberosAttributes where
  parseJSON =
    withObject
      "KerberosAttributes"
      ( \x ->
          KerberosAttributes'
            <$> (x .:? "KdcAdminPassword")
            <*> (x .:? "Realm")
            <*> (x .:? "ADDomainJoinPassword")
            <*> (x .:? "CrossRealmTrustPrincipalPassword")
            <*> (x .:? "ADDomainJoinUser")
      )

instance Hashable KerberosAttributes

instance NFData KerberosAttributes

instance ToJSON KerberosAttributes where
  toJSON KerberosAttributes' {..} =
    object
      ( catMaybes
          [ ("KdcAdminPassword" .=) <$> _kaKdcAdminPassword,
            ("Realm" .=) <$> _kaRealm,
            ("ADDomainJoinPassword" .=) <$> _kaADDomainJoinPassword,
            ("CrossRealmTrustPrincipalPassword" .=)
              <$> _kaCrossRealmTrustPrincipalPassword,
            ("ADDomainJoinUser" .=) <$> _kaADDomainJoinUser
          ]
      )
