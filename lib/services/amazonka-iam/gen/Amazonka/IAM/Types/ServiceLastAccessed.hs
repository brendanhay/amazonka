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
-- Module      : Amazonka.IAM.Types.ServiceLastAccessed
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.ServiceLastAccessed where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types.TrackedActionLastAccessed
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the most recent attempt to access the service.
--
-- This data type is used as a response element in the
-- GetServiceLastAccessedDetails operation.
--
-- /See:/ 'newServiceLastAccessed' smart constructor.
data ServiceLastAccessed = ServiceLastAccessed'
  { -- | The date and time,
    -- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when an
    -- authenticated entity most recently attempted to access the service.
    -- Amazon Web Services does not report unauthenticated requests.
    --
    -- This field is null if no IAM entities attempted to access the service
    -- within the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
    lastAuthenticated :: Prelude.Maybe Data.ISO8601,
    -- | The ARN of the authenticated entity (user or role) that last attempted
    -- to access the service. Amazon Web Services does not report
    -- unauthenticated requests.
    --
    -- This field is null if no IAM entities attempted to access the service
    -- within the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
    lastAuthenticatedEntity :: Prelude.Maybe Prelude.Text,
    -- | The Region from which the authenticated entity (user or role) last
    -- attempted to access the service. Amazon Web Services does not report
    -- unauthenticated requests.
    --
    -- This field is null if no IAM entities attempted to access the service
    -- within the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
    lastAuthenticatedRegion :: Prelude.Maybe Prelude.Text,
    -- | The total number of authenticated principals (root user, IAM users, or
    -- IAM roles) that have attempted to access the service.
    --
    -- This field is null if no principals attempted to access the service
    -- within the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
    totalAuthenticatedEntities :: Prelude.Maybe Prelude.Int,
    -- | An object that contains details about the most recent attempt to access
    -- a tracked action within the service.
    --
    -- This field is null if there no tracked actions or if the principal did
    -- not use the tracked actions within the
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
    -- This field is also null if the report was generated at the service level
    -- and not the action level. For more information, see the @Granularity@
    -- field in GenerateServiceLastAccessedDetails.
    trackedActionsLastAccessed :: Prelude.Maybe [TrackedActionLastAccessed],
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
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces Amazon Web Services Service Namespaces>
    -- in the /Amazon Web Services General Reference/.
    serviceNamespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceLastAccessed' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastAuthenticated', 'serviceLastAccessed_lastAuthenticated' - The date and time,
-- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when an
-- authenticated entity most recently attempted to access the service.
-- Amazon Web Services does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service
-- within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
--
-- 'lastAuthenticatedEntity', 'serviceLastAccessed_lastAuthenticatedEntity' - The ARN of the authenticated entity (user or role) that last attempted
-- to access the service. Amazon Web Services does not report
-- unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service
-- within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
--
-- 'lastAuthenticatedRegion', 'serviceLastAccessed_lastAuthenticatedRegion' - The Region from which the authenticated entity (user or role) last
-- attempted to access the service. Amazon Web Services does not report
-- unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service
-- within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
--
-- 'totalAuthenticatedEntities', 'serviceLastAccessed_totalAuthenticatedEntities' - The total number of authenticated principals (root user, IAM users, or
-- IAM roles) that have attempted to access the service.
--
-- This field is null if no principals attempted to access the service
-- within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
--
-- 'trackedActionsLastAccessed', 'serviceLastAccessed_trackedActionsLastAccessed' - An object that contains details about the most recent attempt to access
-- a tracked action within the service.
--
-- This field is null if there no tracked actions or if the principal did
-- not use the tracked actions within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
-- This field is also null if the report was generated at the service level
-- and not the action level. For more information, see the @Granularity@
-- field in GenerateServiceLastAccessedDetails.
--
-- 'serviceName', 'serviceLastAccessed_serviceName' - The name of the service in which access was attempted.
--
-- 'serviceNamespace', 'serviceLastAccessed_serviceNamespace' - The namespace of the service in which access was attempted.
--
-- To learn the service namespace of a service, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/reference_policies_actions-resources-contextkeys.html Actions, resources, and condition keys for Amazon Web Services services>
-- in the /Service Authorization Reference/. Choose the name of the service
-- to view details for that service. In the first paragraph, find the
-- service prefix. For example, @(service prefix: a4b)@. For more
-- information about service namespaces, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces Amazon Web Services Service Namespaces>
-- in the /Amazon Web Services General Reference/.
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
      { lastAuthenticated =
          Prelude.Nothing,
        lastAuthenticatedEntity = Prelude.Nothing,
        lastAuthenticatedRegion = Prelude.Nothing,
        totalAuthenticatedEntities = Prelude.Nothing,
        trackedActionsLastAccessed = Prelude.Nothing,
        serviceName = pServiceName_,
        serviceNamespace = pServiceNamespace_
      }

-- | The date and time,
-- in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when an
-- authenticated entity most recently attempted to access the service.
-- Amazon Web Services does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service
-- within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
serviceLastAccessed_lastAuthenticated :: Lens.Lens' ServiceLastAccessed (Prelude.Maybe Prelude.UTCTime)
serviceLastAccessed_lastAuthenticated = Lens.lens (\ServiceLastAccessed' {lastAuthenticated} -> lastAuthenticated) (\s@ServiceLastAccessed' {} a -> s {lastAuthenticated = a} :: ServiceLastAccessed) Prelude.. Lens.mapping Data._Time

-- | The ARN of the authenticated entity (user or role) that last attempted
-- to access the service. Amazon Web Services does not report
-- unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service
-- within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
serviceLastAccessed_lastAuthenticatedEntity :: Lens.Lens' ServiceLastAccessed (Prelude.Maybe Prelude.Text)
serviceLastAccessed_lastAuthenticatedEntity = Lens.lens (\ServiceLastAccessed' {lastAuthenticatedEntity} -> lastAuthenticatedEntity) (\s@ServiceLastAccessed' {} a -> s {lastAuthenticatedEntity = a} :: ServiceLastAccessed)

-- | The Region from which the authenticated entity (user or role) last
-- attempted to access the service. Amazon Web Services does not report
-- unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service
-- within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
serviceLastAccessed_lastAuthenticatedRegion :: Lens.Lens' ServiceLastAccessed (Prelude.Maybe Prelude.Text)
serviceLastAccessed_lastAuthenticatedRegion = Lens.lens (\ServiceLastAccessed' {lastAuthenticatedRegion} -> lastAuthenticatedRegion) (\s@ServiceLastAccessed' {} a -> s {lastAuthenticatedRegion = a} :: ServiceLastAccessed)

-- | The total number of authenticated principals (root user, IAM users, or
-- IAM roles) that have attempted to access the service.
--
-- This field is null if no principals attempted to access the service
-- within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
serviceLastAccessed_totalAuthenticatedEntities :: Lens.Lens' ServiceLastAccessed (Prelude.Maybe Prelude.Int)
serviceLastAccessed_totalAuthenticatedEntities = Lens.lens (\ServiceLastAccessed' {totalAuthenticatedEntities} -> totalAuthenticatedEntities) (\s@ServiceLastAccessed' {} a -> s {totalAuthenticatedEntities = a} :: ServiceLastAccessed)

-- | An object that contains details about the most recent attempt to access
-- a tracked action within the service.
--
-- This field is null if there no tracked actions or if the principal did
-- not use the tracked actions within the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period tracking period>.
-- This field is also null if the report was generated at the service level
-- and not the action level. For more information, see the @Granularity@
-- field in GenerateServiceLastAccessedDetails.
serviceLastAccessed_trackedActionsLastAccessed :: Lens.Lens' ServiceLastAccessed (Prelude.Maybe [TrackedActionLastAccessed])
serviceLastAccessed_trackedActionsLastAccessed = Lens.lens (\ServiceLastAccessed' {trackedActionsLastAccessed} -> trackedActionsLastAccessed) (\s@ServiceLastAccessed' {} a -> s {trackedActionsLastAccessed = a} :: ServiceLastAccessed) Prelude.. Lens.mapping Lens.coerced

-- | The name of the service in which access was attempted.
serviceLastAccessed_serviceName :: Lens.Lens' ServiceLastAccessed Prelude.Text
serviceLastAccessed_serviceName = Lens.lens (\ServiceLastAccessed' {serviceName} -> serviceName) (\s@ServiceLastAccessed' {} a -> s {serviceName = a} :: ServiceLastAccessed)

-- | The namespace of the service in which access was attempted.
--
-- To learn the service namespace of a service, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/reference_policies_actions-resources-contextkeys.html Actions, resources, and condition keys for Amazon Web Services services>
-- in the /Service Authorization Reference/. Choose the name of the service
-- to view details for that service. In the first paragraph, find the
-- service prefix. For example, @(service prefix: a4b)@. For more
-- information about service namespaces, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces Amazon Web Services Service Namespaces>
-- in the /Amazon Web Services General Reference/.
serviceLastAccessed_serviceNamespace :: Lens.Lens' ServiceLastAccessed Prelude.Text
serviceLastAccessed_serviceNamespace = Lens.lens (\ServiceLastAccessed' {serviceNamespace} -> serviceNamespace) (\s@ServiceLastAccessed' {} a -> s {serviceNamespace = a} :: ServiceLastAccessed)

instance Data.FromXML ServiceLastAccessed where
  parseXML x =
    ServiceLastAccessed'
      Prelude.<$> (x Data..@? "LastAuthenticated")
      Prelude.<*> (x Data..@? "LastAuthenticatedEntity")
      Prelude.<*> (x Data..@? "LastAuthenticatedRegion")
      Prelude.<*> (x Data..@? "TotalAuthenticatedEntities")
      Prelude.<*> ( x Data..@? "TrackedActionsLastAccessed"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@ "ServiceName")
      Prelude.<*> (x Data..@ "ServiceNamespace")

instance Prelude.Hashable ServiceLastAccessed where
  hashWithSalt _salt ServiceLastAccessed' {..} =
    _salt `Prelude.hashWithSalt` lastAuthenticated
      `Prelude.hashWithSalt` lastAuthenticatedEntity
      `Prelude.hashWithSalt` lastAuthenticatedRegion
      `Prelude.hashWithSalt` totalAuthenticatedEntities
      `Prelude.hashWithSalt` trackedActionsLastAccessed
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` serviceNamespace

instance Prelude.NFData ServiceLastAccessed where
  rnf ServiceLastAccessed' {..} =
    Prelude.rnf lastAuthenticated
      `Prelude.seq` Prelude.rnf lastAuthenticatedEntity
      `Prelude.seq` Prelude.rnf lastAuthenticatedRegion
      `Prelude.seq` Prelude.rnf totalAuthenticatedEntities
      `Prelude.seq` Prelude.rnf trackedActionsLastAccessed
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf serviceNamespace
