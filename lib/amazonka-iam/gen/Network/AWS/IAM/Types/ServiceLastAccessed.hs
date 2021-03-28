{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ServiceLastAccessed
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.ServiceLastAccessed
  ( ServiceLastAccessed (..)
  -- * Smart constructor
  , mkServiceLastAccessed
  -- * Lenses
  , slaServiceName
  , slaServiceNamespace
  , slaLastAuthenticated
  , slaLastAuthenticatedEntity
  , slaLastAuthenticatedRegion
  , slaTotalAuthenticatedEntities
  , slaTrackedActionsLastAccessed
  ) where

import qualified Network.AWS.IAM.Types.LastAuthenticatedEntity as Types
import qualified Network.AWS.IAM.Types.LastAuthenticatedRegion as Types
import qualified Network.AWS.IAM.Types.ServiceName as Types
import qualified Network.AWS.IAM.Types.ServiceNamespace as Types
import qualified Network.AWS.IAM.Types.TrackedActionLastAccessed as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains details about the most recent attempt to access the service.
--
-- This data type is used as a response element in the 'GetServiceLastAccessedDetails' operation.
--
-- /See:/ 'mkServiceLastAccessed' smart constructor.
data ServiceLastAccessed = ServiceLastAccessed'
  { serviceName :: Types.ServiceName
    -- ^ The name of the service in which access was attempted.
  , serviceNamespace :: Types.ServiceNamespace
    -- ^ The namespace of the service in which access was attempted.
--
-- To learn the service namespace of a service, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html Actions, Resources, and Condition Keys for AWS Services> in the /IAM User Guide/ . Choose the name of the service to view details for that service. In the first paragraph, find the service prefix. For example, @(service prefix: a4b)@ . For more information about service namespaces, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /AWS General Reference/ .
  , lastAuthenticated :: Core.Maybe Core.UTCTime
    -- ^ The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when an authenticated entity most recently attempted to access the service. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
  , lastAuthenticatedEntity :: Core.Maybe Types.LastAuthenticatedEntity
    -- ^ The ARN of the authenticated entity (user or role) that last attempted to access the service. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
  , lastAuthenticatedRegion :: Core.Maybe Types.LastAuthenticatedRegion
    -- ^ The Region from which the authenticated entity (user or role) last attempted to access the service. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
  , totalAuthenticatedEntities :: Core.Maybe Core.Int
    -- ^ The total number of authenticated principals (root user, IAM users, or IAM roles) that have attempted to access the service.
--
-- This field is null if no principals attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
  , trackedActionsLastAccessed :: Core.Maybe [Types.TrackedActionLastAccessed]
    -- ^ An object that contains details about the most recent attempt to access a tracked action within the service.
--
-- This field is null if there no tracked actions or if the principal did not use the tracked actions within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> . This field is also null if the report was generated at the service level and not the action level. For more information, see the @Granularity@ field in 'GenerateServiceLastAccessedDetails' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ServiceLastAccessed' value with any optional fields omitted.
mkServiceLastAccessed
    :: Types.ServiceName -- ^ 'serviceName'
    -> Types.ServiceNamespace -- ^ 'serviceNamespace'
    -> ServiceLastAccessed
mkServiceLastAccessed serviceName serviceNamespace
  = ServiceLastAccessed'{serviceName, serviceNamespace,
                         lastAuthenticated = Core.Nothing,
                         lastAuthenticatedEntity = Core.Nothing,
                         lastAuthenticatedRegion = Core.Nothing,
                         totalAuthenticatedEntities = Core.Nothing,
                         trackedActionsLastAccessed = Core.Nothing}

-- | The name of the service in which access was attempted.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slaServiceName :: Lens.Lens' ServiceLastAccessed Types.ServiceName
slaServiceName = Lens.field @"serviceName"
{-# INLINEABLE slaServiceName #-}
{-# DEPRECATED serviceName "Use generic-lens or generic-optics with 'serviceName' instead"  #-}

-- | The namespace of the service in which access was attempted.
--
-- To learn the service namespace of a service, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html Actions, Resources, and Condition Keys for AWS Services> in the /IAM User Guide/ . Choose the name of the service to view details for that service. In the first paragraph, find the service prefix. For example, @(service prefix: a4b)@ . For more information about service namespaces, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'serviceNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slaServiceNamespace :: Lens.Lens' ServiceLastAccessed Types.ServiceNamespace
slaServiceNamespace = Lens.field @"serviceNamespace"
{-# INLINEABLE slaServiceNamespace #-}
{-# DEPRECATED serviceNamespace "Use generic-lens or generic-optics with 'serviceNamespace' instead"  #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when an authenticated entity most recently attempted to access the service. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- /Note:/ Consider using 'lastAuthenticated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slaLastAuthenticated :: Lens.Lens' ServiceLastAccessed (Core.Maybe Core.UTCTime)
slaLastAuthenticated = Lens.field @"lastAuthenticated"
{-# INLINEABLE slaLastAuthenticated #-}
{-# DEPRECATED lastAuthenticated "Use generic-lens or generic-optics with 'lastAuthenticated' instead"  #-}

-- | The ARN of the authenticated entity (user or role) that last attempted to access the service. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- /Note:/ Consider using 'lastAuthenticatedEntity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slaLastAuthenticatedEntity :: Lens.Lens' ServiceLastAccessed (Core.Maybe Types.LastAuthenticatedEntity)
slaLastAuthenticatedEntity = Lens.field @"lastAuthenticatedEntity"
{-# INLINEABLE slaLastAuthenticatedEntity #-}
{-# DEPRECATED lastAuthenticatedEntity "Use generic-lens or generic-optics with 'lastAuthenticatedEntity' instead"  #-}

-- | The Region from which the authenticated entity (user or role) last attempted to access the service. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- /Note:/ Consider using 'lastAuthenticatedRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slaLastAuthenticatedRegion :: Lens.Lens' ServiceLastAccessed (Core.Maybe Types.LastAuthenticatedRegion)
slaLastAuthenticatedRegion = Lens.field @"lastAuthenticatedRegion"
{-# INLINEABLE slaLastAuthenticatedRegion #-}
{-# DEPRECATED lastAuthenticatedRegion "Use generic-lens or generic-optics with 'lastAuthenticatedRegion' instead"  #-}

-- | The total number of authenticated principals (root user, IAM users, or IAM roles) that have attempted to access the service.
--
-- This field is null if no principals attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- /Note:/ Consider using 'totalAuthenticatedEntities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slaTotalAuthenticatedEntities :: Lens.Lens' ServiceLastAccessed (Core.Maybe Core.Int)
slaTotalAuthenticatedEntities = Lens.field @"totalAuthenticatedEntities"
{-# INLINEABLE slaTotalAuthenticatedEntities #-}
{-# DEPRECATED totalAuthenticatedEntities "Use generic-lens or generic-optics with 'totalAuthenticatedEntities' instead"  #-}

-- | An object that contains details about the most recent attempt to access a tracked action within the service.
--
-- This field is null if there no tracked actions or if the principal did not use the tracked actions within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> . This field is also null if the report was generated at the service level and not the action level. For more information, see the @Granularity@ field in 'GenerateServiceLastAccessedDetails' .
--
-- /Note:/ Consider using 'trackedActionsLastAccessed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slaTrackedActionsLastAccessed :: Lens.Lens' ServiceLastAccessed (Core.Maybe [Types.TrackedActionLastAccessed])
slaTrackedActionsLastAccessed = Lens.field @"trackedActionsLastAccessed"
{-# INLINEABLE slaTrackedActionsLastAccessed #-}
{-# DEPRECATED trackedActionsLastAccessed "Use generic-lens or generic-optics with 'trackedActionsLastAccessed' instead"  #-}

instance Core.FromXML ServiceLastAccessed where
        parseXML x
          = ServiceLastAccessed' Core.<$>
              (x Core..@ "ServiceName") Core.<*> x Core..@ "ServiceNamespace"
                Core.<*> x Core..@? "LastAuthenticated"
                Core.<*> x Core..@? "LastAuthenticatedEntity"
                Core.<*> x Core..@? "LastAuthenticatedRegion"
                Core.<*> x Core..@? "TotalAuthenticatedEntities"
                Core.<*>
                x Core..@? "TrackedActionsLastAccessed" Core..<@>
                  Core.parseXMLList "member"
