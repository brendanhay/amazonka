{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.AccessDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.AccessDetail
  ( AccessDetail (..),

    -- * Smart constructor
    mkAccessDetail,

    -- * Lenses
    adEntityPath,
    adServiceNamespace,
    adServiceName,
    adRegion,
    adLastAuthenticatedTime,
    adTotalAuthenticatedEntities,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that contains details about when a principal in the reported AWS Organizations entity last attempted to access an AWS service. A principal can be an IAM user, an IAM role, or the AWS account root user within the reported Organizations entity.
--
-- This data type is a response element in the 'GetOrganizationsAccessReport' operation.
--
-- /See:/ 'mkAccessDetail' smart constructor.
data AccessDetail = AccessDetail'
  { -- | The path of the Organizations entity (root, organizational unit, or account) from which an authenticated principal last attempted to access the service. AWS does not report unauthenticated requests.
    --
    -- This field is null if no principals (IAM users, IAM roles, or root users) in the reported Organizations entity attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
    entityPath :: Lude.Maybe Lude.Text,
    -- | The namespace of the service in which access was attempted.
    --
    -- To learn the service namespace of a service, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html Actions, Resources, and Condition Keys for AWS Services> in the /IAM User Guide/ . Choose the name of the service to view details for that service. In the first paragraph, find the service prefix. For example, @(service prefix: a4b)@ . For more information about service namespaces, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /AWS General Reference/ .
    serviceNamespace :: Lude.Text,
    -- | The name of the service in which access was attempted.
    serviceName :: Lude.Text,
    -- | The Region where the last service access attempt occurred.
    --
    -- This field is null if no principals in the reported Organizations entity attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
    region :: Lude.Maybe Lude.Text,
    -- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when an authenticated principal most recently attempted to access the service. AWS does not report unauthenticated requests.
    --
    -- This field is null if no principals in the reported Organizations entity attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
    lastAuthenticatedTime :: Lude.Maybe Lude.DateTime,
    -- | The number of accounts with authenticated principals (root users, IAM users, and IAM roles) that attempted to access the service in the reporting period.
    totalAuthenticatedEntities :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccessDetail' with the minimum fields required to make a request.
--
-- * 'entityPath' - The path of the Organizations entity (root, organizational unit, or account) from which an authenticated principal last attempted to access the service. AWS does not report unauthenticated requests.
--
-- This field is null if no principals (IAM users, IAM roles, or root users) in the reported Organizations entity attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
-- * 'serviceNamespace' - The namespace of the service in which access was attempted.
--
-- To learn the service namespace of a service, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html Actions, Resources, and Condition Keys for AWS Services> in the /IAM User Guide/ . Choose the name of the service to view details for that service. In the first paragraph, find the service prefix. For example, @(service prefix: a4b)@ . For more information about service namespaces, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /AWS General Reference/ .
-- * 'serviceName' - The name of the service in which access was attempted.
-- * 'region' - The Region where the last service access attempt occurred.
--
-- This field is null if no principals in the reported Organizations entity attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
-- * 'lastAuthenticatedTime' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when an authenticated principal most recently attempted to access the service. AWS does not report unauthenticated requests.
--
-- This field is null if no principals in the reported Organizations entity attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
-- * 'totalAuthenticatedEntities' - The number of accounts with authenticated principals (root users, IAM users, and IAM roles) that attempted to access the service in the reporting period.
mkAccessDetail ::
  -- | 'serviceNamespace'
  Lude.Text ->
  -- | 'serviceName'
  Lude.Text ->
  AccessDetail
mkAccessDetail pServiceNamespace_ pServiceName_ =
  AccessDetail'
    { entityPath = Lude.Nothing,
      serviceNamespace = pServiceNamespace_,
      serviceName = pServiceName_,
      region = Lude.Nothing,
      lastAuthenticatedTime = Lude.Nothing,
      totalAuthenticatedEntities = Lude.Nothing
    }

-- | The path of the Organizations entity (root, organizational unit, or account) from which an authenticated principal last attempted to access the service. AWS does not report unauthenticated requests.
--
-- This field is null if no principals (IAM users, IAM roles, or root users) in the reported Organizations entity attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- /Note:/ Consider using 'entityPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adEntityPath :: Lens.Lens' AccessDetail (Lude.Maybe Lude.Text)
adEntityPath = Lens.lens (entityPath :: AccessDetail -> Lude.Maybe Lude.Text) (\s a -> s {entityPath = a} :: AccessDetail)
{-# DEPRECATED adEntityPath "Use generic-lens or generic-optics with 'entityPath' instead." #-}

-- | The namespace of the service in which access was attempted.
--
-- To learn the service namespace of a service, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html Actions, Resources, and Condition Keys for AWS Services> in the /IAM User Guide/ . Choose the name of the service to view details for that service. In the first paragraph, find the service prefix. For example, @(service prefix: a4b)@ . For more information about service namespaces, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'serviceNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adServiceNamespace :: Lens.Lens' AccessDetail Lude.Text
adServiceNamespace = Lens.lens (serviceNamespace :: AccessDetail -> Lude.Text) (\s a -> s {serviceNamespace = a} :: AccessDetail)
{-# DEPRECATED adServiceNamespace "Use generic-lens or generic-optics with 'serviceNamespace' instead." #-}

-- | The name of the service in which access was attempted.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adServiceName :: Lens.Lens' AccessDetail Lude.Text
adServiceName = Lens.lens (serviceName :: AccessDetail -> Lude.Text) (\s a -> s {serviceName = a} :: AccessDetail)
{-# DEPRECATED adServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | The Region where the last service access attempt occurred.
--
-- This field is null if no principals in the reported Organizations entity attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adRegion :: Lens.Lens' AccessDetail (Lude.Maybe Lude.Text)
adRegion = Lens.lens (region :: AccessDetail -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: AccessDetail)
{-# DEPRECATED adRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when an authenticated principal most recently attempted to access the service. AWS does not report unauthenticated requests.
--
-- This field is null if no principals in the reported Organizations entity attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- /Note:/ Consider using 'lastAuthenticatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adLastAuthenticatedTime :: Lens.Lens' AccessDetail (Lude.Maybe Lude.DateTime)
adLastAuthenticatedTime = Lens.lens (lastAuthenticatedTime :: AccessDetail -> Lude.Maybe Lude.DateTime) (\s a -> s {lastAuthenticatedTime = a} :: AccessDetail)
{-# DEPRECATED adLastAuthenticatedTime "Use generic-lens or generic-optics with 'lastAuthenticatedTime' instead." #-}

-- | The number of accounts with authenticated principals (root users, IAM users, and IAM roles) that attempted to access the service in the reporting period.
--
-- /Note:/ Consider using 'totalAuthenticatedEntities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adTotalAuthenticatedEntities :: Lens.Lens' AccessDetail (Lude.Maybe Lude.Int)
adTotalAuthenticatedEntities = Lens.lens (totalAuthenticatedEntities :: AccessDetail -> Lude.Maybe Lude.Int) (\s a -> s {totalAuthenticatedEntities = a} :: AccessDetail)
{-# DEPRECATED adTotalAuthenticatedEntities "Use generic-lens or generic-optics with 'totalAuthenticatedEntities' instead." #-}

instance Lude.FromXML AccessDetail where
  parseXML x =
    AccessDetail'
      Lude.<$> (x Lude..@? "EntityPath")
      Lude.<*> (x Lude..@ "ServiceNamespace")
      Lude.<*> (x Lude..@ "ServiceName")
      Lude.<*> (x Lude..@? "Region")
      Lude.<*> (x Lude..@? "LastAuthenticatedTime")
      Lude.<*> (x Lude..@? "TotalAuthenticatedEntities")
