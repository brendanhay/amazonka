{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.AccessDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.AccessDetail where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that contains details about when a principal in the reported AWS Organizations entity last attempted to access an AWS service. A principal can be an IAM user, an IAM role, or the AWS account root user within the reported Organizations entity.
--
--
-- This data type is a response element in the 'GetOrganizationsAccessReport' operation.
--
--
-- /See:/ 'accessDetail' smart constructor.
data AccessDetail = AccessDetail'
  { _adEntityPath :: !(Maybe Text),
    _adRegion :: !(Maybe Text),
    _adLastAuthenticatedTime :: !(Maybe ISO8601),
    _adTotalAuthenticatedEntities :: !(Maybe Int),
    _adServiceName :: !Text,
    _adServiceNamespace :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccessDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adEntityPath' - The path of the Organizations entity (root, organizational unit, or account) from which an authenticated principal last attempted to access the service. AWS does not report unauthenticated requests. This field is null if no principals (IAM users, IAM roles, or root users) in the reported Organizations entity attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- * 'adRegion' - The Region where the last service access attempt occurred. This field is null if no principals in the reported Organizations entity attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- * 'adLastAuthenticatedTime' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when an authenticated principal most recently attempted to access the service. AWS does not report unauthenticated requests. This field is null if no principals in the reported Organizations entity attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- * 'adTotalAuthenticatedEntities' - The number of accounts with authenticated principals (root users, IAM users, and IAM roles) that attempted to access the service in the reporting period.
--
-- * 'adServiceName' - The name of the service in which access was attempted.
--
-- * 'adServiceNamespace' - The namespace of the service in which access was attempted. To learn the service namespace of a service, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html Actions, Resources, and Condition Keys for AWS Services> in the /IAM User Guide/ . Choose the name of the service to view details for that service. In the first paragraph, find the service prefix. For example, @(service prefix: a4b)@ . For more information about service namespaces, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /AWS General Reference/ .
accessDetail ::
  -- | 'adServiceName'
  Text ->
  -- | 'adServiceNamespace'
  Text ->
  AccessDetail
accessDetail pServiceName_ pServiceNamespace_ =
  AccessDetail'
    { _adEntityPath = Nothing,
      _adRegion = Nothing,
      _adLastAuthenticatedTime = Nothing,
      _adTotalAuthenticatedEntities = Nothing,
      _adServiceName = pServiceName_,
      _adServiceNamespace = pServiceNamespace_
    }

-- | The path of the Organizations entity (root, organizational unit, or account) from which an authenticated principal last attempted to access the service. AWS does not report unauthenticated requests. This field is null if no principals (IAM users, IAM roles, or root users) in the reported Organizations entity attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
adEntityPath :: Lens' AccessDetail (Maybe Text)
adEntityPath = lens _adEntityPath (\s a -> s {_adEntityPath = a})

-- | The Region where the last service access attempt occurred. This field is null if no principals in the reported Organizations entity attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
adRegion :: Lens' AccessDetail (Maybe Text)
adRegion = lens _adRegion (\s a -> s {_adRegion = a})

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when an authenticated principal most recently attempted to access the service. AWS does not report unauthenticated requests. This field is null if no principals in the reported Organizations entity attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
adLastAuthenticatedTime :: Lens' AccessDetail (Maybe UTCTime)
adLastAuthenticatedTime = lens _adLastAuthenticatedTime (\s a -> s {_adLastAuthenticatedTime = a}) . mapping _Time

-- | The number of accounts with authenticated principals (root users, IAM users, and IAM roles) that attempted to access the service in the reporting period.
adTotalAuthenticatedEntities :: Lens' AccessDetail (Maybe Int)
adTotalAuthenticatedEntities = lens _adTotalAuthenticatedEntities (\s a -> s {_adTotalAuthenticatedEntities = a})

-- | The name of the service in which access was attempted.
adServiceName :: Lens' AccessDetail Text
adServiceName = lens _adServiceName (\s a -> s {_adServiceName = a})

-- | The namespace of the service in which access was attempted. To learn the service namespace of a service, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html Actions, Resources, and Condition Keys for AWS Services> in the /IAM User Guide/ . Choose the name of the service to view details for that service. In the first paragraph, find the service prefix. For example, @(service prefix: a4b)@ . For more information about service namespaces, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /AWS General Reference/ .
adServiceNamespace :: Lens' AccessDetail Text
adServiceNamespace = lens _adServiceNamespace (\s a -> s {_adServiceNamespace = a})

instance FromXML AccessDetail where
  parseXML x =
    AccessDetail'
      <$> (x .@? "EntityPath")
      <*> (x .@? "Region")
      <*> (x .@? "LastAuthenticatedTime")
      <*> (x .@? "TotalAuthenticatedEntities")
      <*> (x .@ "ServiceName")
      <*> (x .@ "ServiceNamespace")

instance Hashable AccessDetail

instance NFData AccessDetail
