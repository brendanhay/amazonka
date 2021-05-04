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
-- Module      : Network.AWS.IAM.Types.ServiceLastAccessed
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ServiceLastAccessed where

import Network.AWS.IAM.Types.TrackedActionLastAccessed
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains details about the most recent attempt to access the service.
--
-- This data type is used as a response element in the
-- GetServiceLastAccessedDetails operation.
--
-- /See:/ 'newServiceLastAccessed' smart constructor.
data ServiceLastAccessed = ServiceLastAccessed'
  { -- | The total number of authenticated principals (root user, IAM users, or
    -- IAM roles) that have attempted to access the service.
    --
    -- This field is null if no principals attempted to access the service
    -- within the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period>.
    totalAuthenticatedEntities :: Prelude.Maybe Prelude.Int,
    -- | The Region from which the authenticated entity (user or role) last
    -- attempted to access the service. AWS does not report unauthenticated
    -- requests.
    --
    -- This field is null if no IAM entities attempted to access the service
    -- within the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period>.
    lastAuthenticatedRegion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the authenticated entity (user or role) that last attempted
    -- to access the service. AWS does not report unauthenticated requests.
    --
    -- This field is null if no IAM entities attempted to access the service
    -- within the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period>.
    lastAuthenticatedEntity :: Prelude.Maybe Prelude.Text,
    -- | An object that contains details about the most recent attempt to access
    -- a tracked action within the service.
    --
    -- This field is null if there no tracked actions or if the principal did
    -- not use the tracked actions within the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period>.
    -- This field is also null if the report was generated at the service level
    -- and not the action level. For more information, see the @Granularity@
    -- field in GenerateServiceLastAccessedDetails.
    trackedActionsLastAccessed :: Prelude.Maybe [TrackedActionLastAccessed],
    -- | The date and time,
    -- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when an
    -- authenticated entity most recently attempted to access the service. AWS
    -- does not report unauthenticated requests.
    --
    -- This field is null if no IAM entities attempted to access the service
    -- within the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period>.
    lastAuthenticated :: Prelude.Maybe Prelude.ISO8601,
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
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces>
    -- in the /AWS General Reference/.
    serviceNamespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ServiceLastAccessed' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalAuthenticatedEntities', 'serviceLastAccessed_totalAuthenticatedEntities' - The total number of authenticated principals (root user, IAM users, or
-- IAM roles) that have attempted to access the service.
--
-- This field is null if no principals attempted to access the service
-- within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period>.
--
-- 'lastAuthenticatedRegion', 'serviceLastAccessed_lastAuthenticatedRegion' - The Region from which the authenticated entity (user or role) last
-- attempted to access the service. AWS does not report unauthenticated
-- requests.
--
-- This field is null if no IAM entities attempted to access the service
-- within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period>.
--
-- 'lastAuthenticatedEntity', 'serviceLastAccessed_lastAuthenticatedEntity' - The ARN of the authenticated entity (user or role) that last attempted
-- to access the service. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service
-- within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period>.
--
-- 'trackedActionsLastAccessed', 'serviceLastAccessed_trackedActionsLastAccessed' - An object that contains details about the most recent attempt to access
-- a tracked action within the service.
--
-- This field is null if there no tracked actions or if the principal did
-- not use the tracked actions within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period>.
-- This field is also null if the report was generated at the service level
-- and not the action level. For more information, see the @Granularity@
-- field in GenerateServiceLastAccessedDetails.
--
-- 'lastAuthenticated', 'serviceLastAccessed_lastAuthenticated' - The date and time,
-- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when an
-- authenticated entity most recently attempted to access the service. AWS
-- does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service
-- within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period>.
--
-- 'serviceName', 'serviceLastAccessed_serviceName' - The name of the service in which access was attempted.
--
-- 'serviceNamespace', 'serviceLastAccessed_serviceNamespace' - The namespace of the service in which access was attempted.
--
-- To learn the service namespace of a service, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/reference_policies_actions-resources-contextkeys.html Actions, resources, and condition keys for AWS services>
-- in the /Service Authorization Reference/. Choose the name of the service
-- to view details for that service. In the first paragraph, find the
-- service prefix. For example, @(service prefix: a4b)@. For more
-- information about service namespaces, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces>
-- in the /AWS General Reference/.
newServiceLastAccessed ::
  -- | 'serviceName'
  Prelude.Text ->
  -- | 'serviceNamespace'
  Prelude.Text ->
  ServiceLastAccessed
newServiceLastAccessed
  pServiceName_
  pServiceNamespace_ =
    ServiceLastAccessed'
      { totalAuthenticatedEntities =
          Prelude.Nothing,
        lastAuthenticatedRegion = Prelude.Nothing,
        lastAuthenticatedEntity = Prelude.Nothing,
        trackedActionsLastAccessed = Prelude.Nothing,
        lastAuthenticated = Prelude.Nothing,
        serviceName = pServiceName_,
        serviceNamespace = pServiceNamespace_
      }

-- | The total number of authenticated principals (root user, IAM users, or
-- IAM roles) that have attempted to access the service.
--
-- This field is null if no principals attempted to access the service
-- within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period>.
serviceLastAccessed_totalAuthenticatedEntities :: Lens.Lens' ServiceLastAccessed (Prelude.Maybe Prelude.Int)
serviceLastAccessed_totalAuthenticatedEntities = Lens.lens (\ServiceLastAccessed' {totalAuthenticatedEntities} -> totalAuthenticatedEntities) (\s@ServiceLastAccessed' {} a -> s {totalAuthenticatedEntities = a} :: ServiceLastAccessed)

-- | The Region from which the authenticated entity (user or role) last
-- attempted to access the service. AWS does not report unauthenticated
-- requests.
--
-- This field is null if no IAM entities attempted to access the service
-- within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period>.
serviceLastAccessed_lastAuthenticatedRegion :: Lens.Lens' ServiceLastAccessed (Prelude.Maybe Prelude.Text)
serviceLastAccessed_lastAuthenticatedRegion = Lens.lens (\ServiceLastAccessed' {lastAuthenticatedRegion} -> lastAuthenticatedRegion) (\s@ServiceLastAccessed' {} a -> s {lastAuthenticatedRegion = a} :: ServiceLastAccessed)

-- | The ARN of the authenticated entity (user or role) that last attempted
-- to access the service. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service
-- within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period>.
serviceLastAccessed_lastAuthenticatedEntity :: Lens.Lens' ServiceLastAccessed (Prelude.Maybe Prelude.Text)
serviceLastAccessed_lastAuthenticatedEntity = Lens.lens (\ServiceLastAccessed' {lastAuthenticatedEntity} -> lastAuthenticatedEntity) (\s@ServiceLastAccessed' {} a -> s {lastAuthenticatedEntity = a} :: ServiceLastAccessed)

-- | An object that contains details about the most recent attempt to access
-- a tracked action within the service.
--
-- This field is null if there no tracked actions or if the principal did
-- not use the tracked actions within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period>.
-- This field is also null if the report was generated at the service level
-- and not the action level. For more information, see the @Granularity@
-- field in GenerateServiceLastAccessedDetails.
serviceLastAccessed_trackedActionsLastAccessed :: Lens.Lens' ServiceLastAccessed (Prelude.Maybe [TrackedActionLastAccessed])
serviceLastAccessed_trackedActionsLastAccessed = Lens.lens (\ServiceLastAccessed' {trackedActionsLastAccessed} -> trackedActionsLastAccessed) (\s@ServiceLastAccessed' {} a -> s {trackedActionsLastAccessed = a} :: ServiceLastAccessed) Prelude.. Lens.mapping Prelude._Coerce

-- | The date and time,
-- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when an
-- authenticated entity most recently attempted to access the service. AWS
-- does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service
-- within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period>.
serviceLastAccessed_lastAuthenticated :: Lens.Lens' ServiceLastAccessed (Prelude.Maybe Prelude.UTCTime)
serviceLastAccessed_lastAuthenticated = Lens.lens (\ServiceLastAccessed' {lastAuthenticated} -> lastAuthenticated) (\s@ServiceLastAccessed' {} a -> s {lastAuthenticated = a} :: ServiceLastAccessed) Prelude.. Lens.mapping Prelude._Time

-- | The name of the service in which access was attempted.
serviceLastAccessed_serviceName :: Lens.Lens' ServiceLastAccessed Prelude.Text
serviceLastAccessed_serviceName = Lens.lens (\ServiceLastAccessed' {serviceName} -> serviceName) (\s@ServiceLastAccessed' {} a -> s {serviceName = a} :: ServiceLastAccessed)

-- | The namespace of the service in which access was attempted.
--
-- To learn the service namespace of a service, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/reference_policies_actions-resources-contextkeys.html Actions, resources, and condition keys for AWS services>
-- in the /Service Authorization Reference/. Choose the name of the service
-- to view details for that service. In the first paragraph, find the
-- service prefix. For example, @(service prefix: a4b)@. For more
-- information about service namespaces, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces>
-- in the /AWS General Reference/.
serviceLastAccessed_serviceNamespace :: Lens.Lens' ServiceLastAccessed Prelude.Text
serviceLastAccessed_serviceNamespace = Lens.lens (\ServiceLastAccessed' {serviceNamespace} -> serviceNamespace) (\s@ServiceLastAccessed' {} a -> s {serviceNamespace = a} :: ServiceLastAccessed)

instance Prelude.FromXML ServiceLastAccessed where
  parseXML x =
    ServiceLastAccessed'
      Prelude.<$> (x Prelude..@? "TotalAuthenticatedEntities")
      Prelude.<*> (x Prelude..@? "LastAuthenticatedRegion")
      Prelude.<*> (x Prelude..@? "LastAuthenticatedEntity")
      Prelude.<*> ( x Prelude..@? "TrackedActionsLastAccessed"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "LastAuthenticated")
      Prelude.<*> (x Prelude..@ "ServiceName")
      Prelude.<*> (x Prelude..@ "ServiceNamespace")

instance Prelude.Hashable ServiceLastAccessed

instance Prelude.NFData ServiceLastAccessed
