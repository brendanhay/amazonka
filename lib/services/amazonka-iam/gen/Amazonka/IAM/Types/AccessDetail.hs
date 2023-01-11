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
-- Module      : Amazonka.IAM.Types.AccessDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.AccessDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that contains details about when a principal in the reported
-- Organizations entity last attempted to access an Amazon Web Services
-- service. A principal can be an IAM user, an IAM role, or the Amazon Web
-- Services account root user within the reported Organizations entity.
--
-- This data type is a response element in the GetOrganizationsAccessReport
-- operation.
--
-- /See:/ 'newAccessDetail' smart constructor.
data AccessDetail = AccessDetail'
  { -- | The path of the Organizations entity (root, organizational unit, or
    -- account) from which an authenticated principal last attempted to access
    -- the service. Amazon Web Services does not report unauthenticated
    -- requests.
    --
    -- This field is null if no principals (IAM users, IAM roles, or root
    -- users) in the reported Organizations entity attempted to access the
    -- service within the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
    entityPath :: Prelude.Maybe Prelude.Text,
    -- | The date and time,
    -- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when an
    -- authenticated principal most recently attempted to access the service.
    -- Amazon Web Services does not report unauthenticated requests.
    --
    -- This field is null if no principals in the reported Organizations entity
    -- attempted to access the service within the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
    lastAuthenticatedTime :: Prelude.Maybe Data.ISO8601,
    -- | The Region where the last service access attempt occurred.
    --
    -- This field is null if no principals in the reported Organizations entity
    -- attempted to access the service within the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
    region :: Prelude.Maybe Prelude.Text,
    -- | The number of accounts with authenticated principals (root users, IAM
    -- users, and IAM roles) that attempted to access the service in the
    -- tracking period.
    totalAuthenticatedEntities :: Prelude.Maybe Prelude.Int,
    -- | The name of the service in which access was attempted.
    serviceName :: Prelude.Text,
    -- | The namespace of the service in which access was attempted.
    --
    -- To learn the service namespace of a service, see
    -- <https://docs.aws.amazon.com/service-authorization/latest/reference/reference_policies_actions-resources-contextkeys.html Actions, resources, and condition keys for Amazon Web Services services>
    -- in the /Service Authorization Reference/. Choose the name of the service
    -- to view details for that service. In the first paragraph, find the
    -- service prefix. For example, @(service prefix: a4b)@. For more
    -- information about service namespaces, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces Amazon Web Services service namespaces>
    -- in the /Amazon Web Services General Reference/.
    serviceNamespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccessDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityPath', 'accessDetail_entityPath' - The path of the Organizations entity (root, organizational unit, or
-- account) from which an authenticated principal last attempted to access
-- the service. Amazon Web Services does not report unauthenticated
-- requests.
--
-- This field is null if no principals (IAM users, IAM roles, or root
-- users) in the reported Organizations entity attempted to access the
-- service within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
--
-- 'lastAuthenticatedTime', 'accessDetail_lastAuthenticatedTime' - The date and time,
-- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when an
-- authenticated principal most recently attempted to access the service.
-- Amazon Web Services does not report unauthenticated requests.
--
-- This field is null if no principals in the reported Organizations entity
-- attempted to access the service within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
--
-- 'region', 'accessDetail_region' - The Region where the last service access attempt occurred.
--
-- This field is null if no principals in the reported Organizations entity
-- attempted to access the service within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
--
-- 'totalAuthenticatedEntities', 'accessDetail_totalAuthenticatedEntities' - The number of accounts with authenticated principals (root users, IAM
-- users, and IAM roles) that attempted to access the service in the
-- tracking period.
--
-- 'serviceName', 'accessDetail_serviceName' - The name of the service in which access was attempted.
--
-- 'serviceNamespace', 'accessDetail_serviceNamespace' - The namespace of the service in which access was attempted.
--
-- To learn the service namespace of a service, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/reference_policies_actions-resources-contextkeys.html Actions, resources, and condition keys for Amazon Web Services services>
-- in the /Service Authorization Reference/. Choose the name of the service
-- to view details for that service. In the first paragraph, find the
-- service prefix. For example, @(service prefix: a4b)@. For more
-- information about service namespaces, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces Amazon Web Services service namespaces>
-- in the /Amazon Web Services General Reference/.
newAccessDetail ::
  -- | 'serviceName'
  Prelude.Text ->
  -- | 'serviceNamespace'
  Prelude.Text ->
  AccessDetail
newAccessDetail pServiceName_ pServiceNamespace_ =
  AccessDetail'
    { entityPath = Prelude.Nothing,
      lastAuthenticatedTime = Prelude.Nothing,
      region = Prelude.Nothing,
      totalAuthenticatedEntities = Prelude.Nothing,
      serviceName = pServiceName_,
      serviceNamespace = pServiceNamespace_
    }

-- | The path of the Organizations entity (root, organizational unit, or
-- account) from which an authenticated principal last attempted to access
-- the service. Amazon Web Services does not report unauthenticated
-- requests.
--
-- This field is null if no principals (IAM users, IAM roles, or root
-- users) in the reported Organizations entity attempted to access the
-- service within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
accessDetail_entityPath :: Lens.Lens' AccessDetail (Prelude.Maybe Prelude.Text)
accessDetail_entityPath = Lens.lens (\AccessDetail' {entityPath} -> entityPath) (\s@AccessDetail' {} a -> s {entityPath = a} :: AccessDetail)

-- | The date and time,
-- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when an
-- authenticated principal most recently attempted to access the service.
-- Amazon Web Services does not report unauthenticated requests.
--
-- This field is null if no principals in the reported Organizations entity
-- attempted to access the service within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
accessDetail_lastAuthenticatedTime :: Lens.Lens' AccessDetail (Prelude.Maybe Prelude.UTCTime)
accessDetail_lastAuthenticatedTime = Lens.lens (\AccessDetail' {lastAuthenticatedTime} -> lastAuthenticatedTime) (\s@AccessDetail' {} a -> s {lastAuthenticatedTime = a} :: AccessDetail) Prelude.. Lens.mapping Data._Time

-- | The Region where the last service access attempt occurred.
--
-- This field is null if no principals in the reported Organizations entity
-- attempted to access the service within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
accessDetail_region :: Lens.Lens' AccessDetail (Prelude.Maybe Prelude.Text)
accessDetail_region = Lens.lens (\AccessDetail' {region} -> region) (\s@AccessDetail' {} a -> s {region = a} :: AccessDetail)

-- | The number of accounts with authenticated principals (root users, IAM
-- users, and IAM roles) that attempted to access the service in the
-- tracking period.
accessDetail_totalAuthenticatedEntities :: Lens.Lens' AccessDetail (Prelude.Maybe Prelude.Int)
accessDetail_totalAuthenticatedEntities = Lens.lens (\AccessDetail' {totalAuthenticatedEntities} -> totalAuthenticatedEntities) (\s@AccessDetail' {} a -> s {totalAuthenticatedEntities = a} :: AccessDetail)

-- | The name of the service in which access was attempted.
accessDetail_serviceName :: Lens.Lens' AccessDetail Prelude.Text
accessDetail_serviceName = Lens.lens (\AccessDetail' {serviceName} -> serviceName) (\s@AccessDetail' {} a -> s {serviceName = a} :: AccessDetail)

-- | The namespace of the service in which access was attempted.
--
-- To learn the service namespace of a service, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/reference_policies_actions-resources-contextkeys.html Actions, resources, and condition keys for Amazon Web Services services>
-- in the /Service Authorization Reference/. Choose the name of the service
-- to view details for that service. In the first paragraph, find the
-- service prefix. For example, @(service prefix: a4b)@. For more
-- information about service namespaces, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces Amazon Web Services service namespaces>
-- in the /Amazon Web Services General Reference/.
accessDetail_serviceNamespace :: Lens.Lens' AccessDetail Prelude.Text
accessDetail_serviceNamespace = Lens.lens (\AccessDetail' {serviceNamespace} -> serviceNamespace) (\s@AccessDetail' {} a -> s {serviceNamespace = a} :: AccessDetail)

instance Data.FromXML AccessDetail where
  parseXML x =
    AccessDetail'
      Prelude.<$> (x Data..@? "EntityPath")
      Prelude.<*> (x Data..@? "LastAuthenticatedTime")
      Prelude.<*> (x Data..@? "Region")
      Prelude.<*> (x Data..@? "TotalAuthenticatedEntities")
      Prelude.<*> (x Data..@ "ServiceName")
      Prelude.<*> (x Data..@ "ServiceNamespace")

instance Prelude.Hashable AccessDetail where
  hashWithSalt _salt AccessDetail' {..} =
    _salt `Prelude.hashWithSalt` entityPath
      `Prelude.hashWithSalt` lastAuthenticatedTime
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` totalAuthenticatedEntities
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` serviceNamespace

instance Prelude.NFData AccessDetail where
  rnf AccessDetail' {..} =
    Prelude.rnf entityPath
      `Prelude.seq` Prelude.rnf lastAuthenticatedTime
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf totalAuthenticatedEntities
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf serviceNamespace
