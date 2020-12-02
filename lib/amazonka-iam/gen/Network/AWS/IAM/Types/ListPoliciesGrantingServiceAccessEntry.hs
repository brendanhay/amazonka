{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ListPoliciesGrantingServiceAccessEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ListPoliciesGrantingServiceAccessEntry where

import Network.AWS.IAM.Types.PolicyGrantingServiceAccess
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about the permissions policies that are attached to the specified identity (user, group, or role).
--
--
-- This data type is used as a response element in the 'ListPoliciesGrantingServiceAccess' operation.
--
--
-- /See:/ 'listPoliciesGrantingServiceAccessEntry' smart constructor.
data ListPoliciesGrantingServiceAccessEntry = ListPoliciesGrantingServiceAccessEntry'
  { _lpgsaeServiceNamespace ::
      !(Maybe Text),
    _lpgsaePolicies ::
      !( Maybe
           [PolicyGrantingServiceAccess]
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListPoliciesGrantingServiceAccessEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpgsaeServiceNamespace' - The namespace of the service that was accessed. To learn the service namespace of a service, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html Actions, Resources, and Condition Keys for AWS Services> in the /IAM User Guide/ . Choose the name of the service to view details for that service. In the first paragraph, find the service prefix. For example, @(service prefix: a4b)@ . For more information about service namespaces, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /AWS General Reference/ .
--
-- * 'lpgsaePolicies' - The @PoliciesGrantingServiceAccess@ object that contains details about the policy.
listPoliciesGrantingServiceAccessEntry ::
  ListPoliciesGrantingServiceAccessEntry
listPoliciesGrantingServiceAccessEntry =
  ListPoliciesGrantingServiceAccessEntry'
    { _lpgsaeServiceNamespace =
        Nothing,
      _lpgsaePolicies = Nothing
    }

-- | The namespace of the service that was accessed. To learn the service namespace of a service, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html Actions, Resources, and Condition Keys for AWS Services> in the /IAM User Guide/ . Choose the name of the service to view details for that service. In the first paragraph, find the service prefix. For example, @(service prefix: a4b)@ . For more information about service namespaces, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /AWS General Reference/ .
lpgsaeServiceNamespace :: Lens' ListPoliciesGrantingServiceAccessEntry (Maybe Text)
lpgsaeServiceNamespace = lens _lpgsaeServiceNamespace (\s a -> s {_lpgsaeServiceNamespace = a})

-- | The @PoliciesGrantingServiceAccess@ object that contains details about the policy.
lpgsaePolicies :: Lens' ListPoliciesGrantingServiceAccessEntry [PolicyGrantingServiceAccess]
lpgsaePolicies = lens _lpgsaePolicies (\s a -> s {_lpgsaePolicies = a}) . _Default . _Coerce

instance FromXML ListPoliciesGrantingServiceAccessEntry where
  parseXML x =
    ListPoliciesGrantingServiceAccessEntry'
      <$> (x .@? "ServiceNamespace")
      <*> (x .@? "Policies" .!@ mempty >>= may (parseXMLList "member"))

instance Hashable ListPoliciesGrantingServiceAccessEntry

instance NFData ListPoliciesGrantingServiceAccessEntry
