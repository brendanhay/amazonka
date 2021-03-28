{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.AccessDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.AccessDetail
  ( AccessDetail (..)
  -- * Smart constructor
  , mkAccessDetail
  -- * Lenses
  , adServiceName
  , adServiceNamespace
  , adEntityPath
  , adLastAuthenticatedTime
  , adRegion
  , adTotalAuthenticatedEntities
  ) where

import qualified Network.AWS.IAM.Types.OrganizationsEntityPathType as Types
import qualified Network.AWS.IAM.Types.ServiceNameType as Types
import qualified Network.AWS.IAM.Types.ServiceNamespace as Types
import qualified Network.AWS.IAM.Types.StringType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that contains details about when a principal in the reported AWS Organizations entity last attempted to access an AWS service. A principal can be an IAM user, an IAM role, or the AWS account root user within the reported Organizations entity.
--
-- This data type is a response element in the 'GetOrganizationsAccessReport' operation.
--
-- /See:/ 'mkAccessDetail' smart constructor.
data AccessDetail = AccessDetail'
  { serviceName :: Types.ServiceNameType
    -- ^ The name of the service in which access was attempted.
  , serviceNamespace :: Types.ServiceNamespace
    -- ^ The namespace of the service in which access was attempted.
--
-- To learn the service namespace of a service, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html Actions, Resources, and Condition Keys for AWS Services> in the /IAM User Guide/ . Choose the name of the service to view details for that service. In the first paragraph, find the service prefix. For example, @(service prefix: a4b)@ . For more information about service namespaces, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /AWS General Reference/ .
  , entityPath :: Core.Maybe Types.OrganizationsEntityPathType
    -- ^ The path of the Organizations entity (root, organizational unit, or account) from which an authenticated principal last attempted to access the service. AWS does not report unauthenticated requests.
--
-- This field is null if no principals (IAM users, IAM roles, or root users) in the reported Organizations entity attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
  , lastAuthenticatedTime :: Core.Maybe Core.UTCTime
    -- ^ The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when an authenticated principal most recently attempted to access the service. AWS does not report unauthenticated requests.
--
-- This field is null if no principals in the reported Organizations entity attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
  , region :: Core.Maybe Types.StringType
    -- ^ The Region where the last service access attempt occurred.
--
-- This field is null if no principals in the reported Organizations entity attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
  , totalAuthenticatedEntities :: Core.Maybe Core.Int
    -- ^ The number of accounts with authenticated principals (root users, IAM users, and IAM roles) that attempted to access the service in the reporting period.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'AccessDetail' value with any optional fields omitted.
mkAccessDetail
    :: Types.ServiceNameType -- ^ 'serviceName'
    -> Types.ServiceNamespace -- ^ 'serviceNamespace'
    -> AccessDetail
mkAccessDetail serviceName serviceNamespace
  = AccessDetail'{serviceName, serviceNamespace,
                  entityPath = Core.Nothing, lastAuthenticatedTime = Core.Nothing,
                  region = Core.Nothing, totalAuthenticatedEntities = Core.Nothing}

-- | The name of the service in which access was attempted.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adServiceName :: Lens.Lens' AccessDetail Types.ServiceNameType
adServiceName = Lens.field @"serviceName"
{-# INLINEABLE adServiceName #-}
{-# DEPRECATED serviceName "Use generic-lens or generic-optics with 'serviceName' instead"  #-}

-- | The namespace of the service in which access was attempted.
--
-- To learn the service namespace of a service, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html Actions, Resources, and Condition Keys for AWS Services> in the /IAM User Guide/ . Choose the name of the service to view details for that service. In the first paragraph, find the service prefix. For example, @(service prefix: a4b)@ . For more information about service namespaces, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'serviceNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adServiceNamespace :: Lens.Lens' AccessDetail Types.ServiceNamespace
adServiceNamespace = Lens.field @"serviceNamespace"
{-# INLINEABLE adServiceNamespace #-}
{-# DEPRECATED serviceNamespace "Use generic-lens or generic-optics with 'serviceNamespace' instead"  #-}

-- | The path of the Organizations entity (root, organizational unit, or account) from which an authenticated principal last attempted to access the service. AWS does not report unauthenticated requests.
--
-- This field is null if no principals (IAM users, IAM roles, or root users) in the reported Organizations entity attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- /Note:/ Consider using 'entityPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adEntityPath :: Lens.Lens' AccessDetail (Core.Maybe Types.OrganizationsEntityPathType)
adEntityPath = Lens.field @"entityPath"
{-# INLINEABLE adEntityPath #-}
{-# DEPRECATED entityPath "Use generic-lens or generic-optics with 'entityPath' instead"  #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when an authenticated principal most recently attempted to access the service. AWS does not report unauthenticated requests.
--
-- This field is null if no principals in the reported Organizations entity attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- /Note:/ Consider using 'lastAuthenticatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adLastAuthenticatedTime :: Lens.Lens' AccessDetail (Core.Maybe Core.UTCTime)
adLastAuthenticatedTime = Lens.field @"lastAuthenticatedTime"
{-# INLINEABLE adLastAuthenticatedTime #-}
{-# DEPRECATED lastAuthenticatedTime "Use generic-lens or generic-optics with 'lastAuthenticatedTime' instead"  #-}

-- | The Region where the last service access attempt occurred.
--
-- This field is null if no principals in the reported Organizations entity attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adRegion :: Lens.Lens' AccessDetail (Core.Maybe Types.StringType)
adRegion = Lens.field @"region"
{-# INLINEABLE adRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

-- | The number of accounts with authenticated principals (root users, IAM users, and IAM roles) that attempted to access the service in the reporting period.
--
-- /Note:/ Consider using 'totalAuthenticatedEntities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adTotalAuthenticatedEntities :: Lens.Lens' AccessDetail (Core.Maybe Core.Int)
adTotalAuthenticatedEntities = Lens.field @"totalAuthenticatedEntities"
{-# INLINEABLE adTotalAuthenticatedEntities #-}
{-# DEPRECATED totalAuthenticatedEntities "Use generic-lens or generic-optics with 'totalAuthenticatedEntities' instead"  #-}

instance Core.FromXML AccessDetail where
        parseXML x
          = AccessDetail' Core.<$>
              (x Core..@ "ServiceName") Core.<*> x Core..@ "ServiceNamespace"
                Core.<*> x Core..@? "EntityPath"
                Core.<*> x Core..@? "LastAuthenticatedTime"
                Core.<*> x Core..@? "Region"
                Core.<*> x Core..@? "TotalAuthenticatedEntities"
