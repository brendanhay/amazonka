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
-- Module      : Network.AWS.IAM.Types.AccessDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.AccessDetail where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that contains details about when a principal in the reported
-- AWS Organizations entity last attempted to access an AWS service. A
-- principal can be an IAM user, an IAM role, or the AWS account root user
-- within the reported Organizations entity.
--
-- This data type is a response element in the GetOrganizationsAccessReport
-- operation.
--
-- /See:/ 'newAccessDetail' smart constructor.
data AccessDetail = AccessDetail'
  { -- | The number of accounts with authenticated principals (root users, IAM
    -- users, and IAM roles) that attempted to access the service in the
    -- reporting period.
    totalAuthenticatedEntities :: Prelude.Maybe Prelude.Int,
    -- | The path of the Organizations entity (root, organizational unit, or
    -- account) from which an authenticated principal last attempted to access
    -- the service. AWS does not report unauthenticated requests.
    --
    -- This field is null if no principals (IAM users, IAM roles, or root
    -- users) in the reported Organizations entity attempted to access the
    -- service within the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period>.
    entityPath :: Prelude.Maybe Prelude.Text,
    -- | The date and time,
    -- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when an
    -- authenticated principal most recently attempted to access the service.
    -- AWS does not report unauthenticated requests.
    --
    -- This field is null if no principals in the reported Organizations entity
    -- attempted to access the service within the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period>.
    lastAuthenticatedTime :: Prelude.Maybe Prelude.ISO8601,
    -- | The Region where the last service access attempt occurred.
    --
    -- This field is null if no principals in the reported Organizations entity
    -- attempted to access the service within the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period>.
    region :: Prelude.Maybe Prelude.Text,
    -- | The name of the service in which access was attempted.
    serviceName :: Prelude.Text,
    -- | The namespace of the service in which access was attempted.
    --
    -- To learn the service namespace of a service, see
    -- <https://docs.aws.amazon.com/service-authorization/latest/reference/reference_policies_actions-resources-contextkeys.html Actions, resources, and condition keys for AWS services>
    -- in the /Service Authorization Reference/. Choose the name of the service
    -- to view details for that service. In the first paragraph, find the
    -- service prefix. For example, @(service prefix: a4b)@. For more
    -- information about service namespaces, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS service namespaces>
    -- in the /AWS General Reference/.
    serviceNamespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AccessDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalAuthenticatedEntities', 'accessDetail_totalAuthenticatedEntities' - The number of accounts with authenticated principals (root users, IAM
-- users, and IAM roles) that attempted to access the service in the
-- reporting period.
--
-- 'entityPath', 'accessDetail_entityPath' - The path of the Organizations entity (root, organizational unit, or
-- account) from which an authenticated principal last attempted to access
-- the service. AWS does not report unauthenticated requests.
--
-- This field is null if no principals (IAM users, IAM roles, or root
-- users) in the reported Organizations entity attempted to access the
-- service within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period>.
--
-- 'lastAuthenticatedTime', 'accessDetail_lastAuthenticatedTime' - The date and time,
-- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when an
-- authenticated principal most recently attempted to access the service.
-- AWS does not report unauthenticated requests.
--
-- This field is null if no principals in the reported Organizations entity
-- attempted to access the service within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period>.
--
-- 'region', 'accessDetail_region' - The Region where the last service access attempt occurred.
--
-- This field is null if no principals in the reported Organizations entity
-- attempted to access the service within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period>.
--
-- 'serviceName', 'accessDetail_serviceName' - The name of the service in which access was attempted.
--
-- 'serviceNamespace', 'accessDetail_serviceNamespace' - The namespace of the service in which access was attempted.
--
-- To learn the service namespace of a service, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/reference_policies_actions-resources-contextkeys.html Actions, resources, and condition keys for AWS services>
-- in the /Service Authorization Reference/. Choose the name of the service
-- to view details for that service. In the first paragraph, find the
-- service prefix. For example, @(service prefix: a4b)@. For more
-- information about service namespaces, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS service namespaces>
-- in the /AWS General Reference/.
newAccessDetail ::
  -- | 'serviceName'
  Prelude.Text ->
  -- | 'serviceNamespace'
  Prelude.Text ->
  AccessDetail
newAccessDetail pServiceName_ pServiceNamespace_ =
  AccessDetail'
    { totalAuthenticatedEntities =
        Prelude.Nothing,
      entityPath = Prelude.Nothing,
      lastAuthenticatedTime = Prelude.Nothing,
      region = Prelude.Nothing,
      serviceName = pServiceName_,
      serviceNamespace = pServiceNamespace_
    }

-- | The number of accounts with authenticated principals (root users, IAM
-- users, and IAM roles) that attempted to access the service in the
-- reporting period.
accessDetail_totalAuthenticatedEntities :: Lens.Lens' AccessDetail (Prelude.Maybe Prelude.Int)
accessDetail_totalAuthenticatedEntities = Lens.lens (\AccessDetail' {totalAuthenticatedEntities} -> totalAuthenticatedEntities) (\s@AccessDetail' {} a -> s {totalAuthenticatedEntities = a} :: AccessDetail)

-- | The path of the Organizations entity (root, organizational unit, or
-- account) from which an authenticated principal last attempted to access
-- the service. AWS does not report unauthenticated requests.
--
-- This field is null if no principals (IAM users, IAM roles, or root
-- users) in the reported Organizations entity attempted to access the
-- service within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period>.
accessDetail_entityPath :: Lens.Lens' AccessDetail (Prelude.Maybe Prelude.Text)
accessDetail_entityPath = Lens.lens (\AccessDetail' {entityPath} -> entityPath) (\s@AccessDetail' {} a -> s {entityPath = a} :: AccessDetail)

-- | The date and time,
-- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when an
-- authenticated principal most recently attempted to access the service.
-- AWS does not report unauthenticated requests.
--
-- This field is null if no principals in the reported Organizations entity
-- attempted to access the service within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period>.
accessDetail_lastAuthenticatedTime :: Lens.Lens' AccessDetail (Prelude.Maybe Prelude.UTCTime)
accessDetail_lastAuthenticatedTime = Lens.lens (\AccessDetail' {lastAuthenticatedTime} -> lastAuthenticatedTime) (\s@AccessDetail' {} a -> s {lastAuthenticatedTime = a} :: AccessDetail) Prelude.. Lens.mapping Prelude._Time

-- | The Region where the last service access attempt occurred.
--
-- This field is null if no principals in the reported Organizations entity
-- attempted to access the service within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period>.
accessDetail_region :: Lens.Lens' AccessDetail (Prelude.Maybe Prelude.Text)
accessDetail_region = Lens.lens (\AccessDetail' {region} -> region) (\s@AccessDetail' {} a -> s {region = a} :: AccessDetail)

-- | The name of the service in which access was attempted.
accessDetail_serviceName :: Lens.Lens' AccessDetail Prelude.Text
accessDetail_serviceName = Lens.lens (\AccessDetail' {serviceName} -> serviceName) (\s@AccessDetail' {} a -> s {serviceName = a} :: AccessDetail)

-- | The namespace of the service in which access was attempted.
--
-- To learn the service namespace of a service, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/reference_policies_actions-resources-contextkeys.html Actions, resources, and condition keys for AWS services>
-- in the /Service Authorization Reference/. Choose the name of the service
-- to view details for that service. In the first paragraph, find the
-- service prefix. For example, @(service prefix: a4b)@. For more
-- information about service namespaces, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS service namespaces>
-- in the /AWS General Reference/.
accessDetail_serviceNamespace :: Lens.Lens' AccessDetail Prelude.Text
accessDetail_serviceNamespace = Lens.lens (\AccessDetail' {serviceNamespace} -> serviceNamespace) (\s@AccessDetail' {} a -> s {serviceNamespace = a} :: AccessDetail)

instance Prelude.FromXML AccessDetail where
  parseXML x =
    AccessDetail'
      Prelude.<$> (x Prelude..@? "TotalAuthenticatedEntities")
      Prelude.<*> (x Prelude..@? "EntityPath")
      Prelude.<*> (x Prelude..@? "LastAuthenticatedTime")
      Prelude.<*> (x Prelude..@? "Region")
      Prelude.<*> (x Prelude..@ "ServiceName")
      Prelude.<*> (x Prelude..@ "ServiceNamespace")

instance Prelude.Hashable AccessDetail

instance Prelude.NFData AccessDetail
