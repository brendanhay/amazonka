{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ServiceLastAccessed
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ServiceLastAccessed
  ( ServiceLastAccessed (..),

    -- * Smart constructor
    mkServiceLastAccessed,

    -- * Lenses
    slaLastAuthenticated,
    slaTrackedActionsLastAccessed,
    slaLastAuthenticatedEntity,
    slaLastAuthenticatedRegion,
    slaTotalAuthenticatedEntities,
    slaServiceName,
    slaServiceNamespace,
  )
where

import Network.AWS.IAM.Types.TrackedActionLastAccessed
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about the most recent attempt to access the service.
--
-- This data type is used as a response element in the 'GetServiceLastAccessedDetails' operation.
--
-- /See:/ 'mkServiceLastAccessed' smart constructor.
data ServiceLastAccessed = ServiceLastAccessed'
  { lastAuthenticated ::
      Lude.Maybe Lude.DateTime,
    trackedActionsLastAccessed ::
      Lude.Maybe [TrackedActionLastAccessed],
    lastAuthenticatedEntity :: Lude.Maybe Lude.Text,
    lastAuthenticatedRegion :: Lude.Maybe Lude.Text,
    totalAuthenticatedEntities :: Lude.Maybe Lude.Int,
    serviceName :: Lude.Text,
    serviceNamespace :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceLastAccessed' with the minimum fields required to make a request.
--
-- * 'lastAuthenticated' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when an authenticated entity most recently attempted to access the service. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
-- * 'lastAuthenticatedEntity' - The ARN of the authenticated entity (user or role) that last attempted to access the service. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
-- * 'lastAuthenticatedRegion' - The Region from which the authenticated entity (user or role) last attempted to access the service. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
-- * 'serviceName' - The name of the service in which access was attempted.
-- * 'serviceNamespace' - The namespace of the service in which access was attempted.
--
-- To learn the service namespace of a service, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html Actions, Resources, and Condition Keys for AWS Services> in the /IAM User Guide/ . Choose the name of the service to view details for that service. In the first paragraph, find the service prefix. For example, @(service prefix: a4b)@ . For more information about service namespaces, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /AWS General Reference/ .
-- * 'totalAuthenticatedEntities' - The total number of authenticated principals (root user, IAM users, or IAM roles) that have attempted to access the service.
--
-- This field is null if no principals attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
-- * 'trackedActionsLastAccessed' - An object that contains details about the most recent attempt to access a tracked action within the service.
--
-- This field is null if there no tracked actions or if the principal did not use the tracked actions within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> . This field is also null if the report was generated at the service level and not the action level. For more information, see the @Granularity@ field in 'GenerateServiceLastAccessedDetails' .
mkServiceLastAccessed ::
  -- | 'serviceName'
  Lude.Text ->
  -- | 'serviceNamespace'
  Lude.Text ->
  ServiceLastAccessed
mkServiceLastAccessed pServiceName_ pServiceNamespace_ =
  ServiceLastAccessed'
    { lastAuthenticated = Lude.Nothing,
      trackedActionsLastAccessed = Lude.Nothing,
      lastAuthenticatedEntity = Lude.Nothing,
      lastAuthenticatedRegion = Lude.Nothing,
      totalAuthenticatedEntities = Lude.Nothing,
      serviceName = pServiceName_,
      serviceNamespace = pServiceNamespace_
    }

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when an authenticated entity most recently attempted to access the service. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- /Note:/ Consider using 'lastAuthenticated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slaLastAuthenticated :: Lens.Lens' ServiceLastAccessed (Lude.Maybe Lude.DateTime)
slaLastAuthenticated = Lens.lens (lastAuthenticated :: ServiceLastAccessed -> Lude.Maybe Lude.DateTime) (\s a -> s {lastAuthenticated = a} :: ServiceLastAccessed)
{-# DEPRECATED slaLastAuthenticated "Use generic-lens or generic-optics with 'lastAuthenticated' instead." #-}

-- | An object that contains details about the most recent attempt to access a tracked action within the service.
--
-- This field is null if there no tracked actions or if the principal did not use the tracked actions within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> . This field is also null if the report was generated at the service level and not the action level. For more information, see the @Granularity@ field in 'GenerateServiceLastAccessedDetails' .
--
-- /Note:/ Consider using 'trackedActionsLastAccessed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slaTrackedActionsLastAccessed :: Lens.Lens' ServiceLastAccessed (Lude.Maybe [TrackedActionLastAccessed])
slaTrackedActionsLastAccessed = Lens.lens (trackedActionsLastAccessed :: ServiceLastAccessed -> Lude.Maybe [TrackedActionLastAccessed]) (\s a -> s {trackedActionsLastAccessed = a} :: ServiceLastAccessed)
{-# DEPRECATED slaTrackedActionsLastAccessed "Use generic-lens or generic-optics with 'trackedActionsLastAccessed' instead." #-}

-- | The ARN of the authenticated entity (user or role) that last attempted to access the service. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- /Note:/ Consider using 'lastAuthenticatedEntity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slaLastAuthenticatedEntity :: Lens.Lens' ServiceLastAccessed (Lude.Maybe Lude.Text)
slaLastAuthenticatedEntity = Lens.lens (lastAuthenticatedEntity :: ServiceLastAccessed -> Lude.Maybe Lude.Text) (\s a -> s {lastAuthenticatedEntity = a} :: ServiceLastAccessed)
{-# DEPRECATED slaLastAuthenticatedEntity "Use generic-lens or generic-optics with 'lastAuthenticatedEntity' instead." #-}

-- | The Region from which the authenticated entity (user or role) last attempted to access the service. AWS does not report unauthenticated requests.
--
-- This field is null if no IAM entities attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- /Note:/ Consider using 'lastAuthenticatedRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slaLastAuthenticatedRegion :: Lens.Lens' ServiceLastAccessed (Lude.Maybe Lude.Text)
slaLastAuthenticatedRegion = Lens.lens (lastAuthenticatedRegion :: ServiceLastAccessed -> Lude.Maybe Lude.Text) (\s a -> s {lastAuthenticatedRegion = a} :: ServiceLastAccessed)
{-# DEPRECATED slaLastAuthenticatedRegion "Use generic-lens or generic-optics with 'lastAuthenticatedRegion' instead." #-}

-- | The total number of authenticated principals (root user, IAM users, or IAM roles) that have attempted to access the service.
--
-- This field is null if no principals attempted to access the service within the <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_access-advisor.html#service-last-accessed-reporting-period reporting period> .
--
-- /Note:/ Consider using 'totalAuthenticatedEntities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slaTotalAuthenticatedEntities :: Lens.Lens' ServiceLastAccessed (Lude.Maybe Lude.Int)
slaTotalAuthenticatedEntities = Lens.lens (totalAuthenticatedEntities :: ServiceLastAccessed -> Lude.Maybe Lude.Int) (\s a -> s {totalAuthenticatedEntities = a} :: ServiceLastAccessed)
{-# DEPRECATED slaTotalAuthenticatedEntities "Use generic-lens or generic-optics with 'totalAuthenticatedEntities' instead." #-}

-- | The name of the service in which access was attempted.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slaServiceName :: Lens.Lens' ServiceLastAccessed Lude.Text
slaServiceName = Lens.lens (serviceName :: ServiceLastAccessed -> Lude.Text) (\s a -> s {serviceName = a} :: ServiceLastAccessed)
{-# DEPRECATED slaServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | The namespace of the service in which access was attempted.
--
-- To learn the service namespace of a service, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html Actions, Resources, and Condition Keys for AWS Services> in the /IAM User Guide/ . Choose the name of the service to view details for that service. In the first paragraph, find the service prefix. For example, @(service prefix: a4b)@ . For more information about service namespaces, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'serviceNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slaServiceNamespace :: Lens.Lens' ServiceLastAccessed Lude.Text
slaServiceNamespace = Lens.lens (serviceNamespace :: ServiceLastAccessed -> Lude.Text) (\s a -> s {serviceNamespace = a} :: ServiceLastAccessed)
{-# DEPRECATED slaServiceNamespace "Use generic-lens or generic-optics with 'serviceNamespace' instead." #-}

instance Lude.FromXML ServiceLastAccessed where
  parseXML x =
    ServiceLastAccessed'
      Lude.<$> (x Lude..@? "LastAuthenticated")
      Lude.<*> ( x Lude..@? "TrackedActionsLastAccessed" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "LastAuthenticatedEntity")
      Lude.<*> (x Lude..@? "LastAuthenticatedRegion")
      Lude.<*> (x Lude..@? "TotalAuthenticatedEntities")
      Lude.<*> (x Lude..@ "ServiceName")
      Lude.<*> (x Lude..@ "ServiceNamespace")
