{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ListPoliciesGrantingServiceAccessEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ListPoliciesGrantingServiceAccessEntry
  ( ListPoliciesGrantingServiceAccessEntry (..),

    -- * Smart constructor
    mkListPoliciesGrantingServiceAccessEntry,

    -- * Lenses
    lpgsaePolicies,
    lpgsaeServiceNamespace,
  )
where

import qualified Network.AWS.IAM.Types.PolicyGrantingServiceAccess as Types
import qualified Network.AWS.IAM.Types.ServiceNamespace as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains details about the permissions policies that are attached to the specified identity (user, group, or role).
--
-- This data type is used as a response element in the 'ListPoliciesGrantingServiceAccess' operation.
--
-- /See:/ 'mkListPoliciesGrantingServiceAccessEntry' smart constructor.
data ListPoliciesGrantingServiceAccessEntry = ListPoliciesGrantingServiceAccessEntry'
  { -- | The @PoliciesGrantingServiceAccess@ object that contains details about the policy.
    policies :: Core.Maybe [Types.PolicyGrantingServiceAccess],
    -- | The namespace of the service that was accessed.
    --
    -- To learn the service namespace of a service, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html Actions, Resources, and Condition Keys for AWS Services> in the /IAM User Guide/ . Choose the name of the service to view details for that service. In the first paragraph, find the service prefix. For example, @(service prefix: a4b)@ . For more information about service namespaces, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /AWS General Reference/ .
    serviceNamespace :: Core.Maybe Types.ServiceNamespace
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPoliciesGrantingServiceAccessEntry' value with any optional fields omitted.
mkListPoliciesGrantingServiceAccessEntry ::
  ListPoliciesGrantingServiceAccessEntry
mkListPoliciesGrantingServiceAccessEntry =
  ListPoliciesGrantingServiceAccessEntry'
    { policies = Core.Nothing,
      serviceNamespace = Core.Nothing
    }

-- | The @PoliciesGrantingServiceAccess@ object that contains details about the policy.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpgsaePolicies :: Lens.Lens' ListPoliciesGrantingServiceAccessEntry (Core.Maybe [Types.PolicyGrantingServiceAccess])
lpgsaePolicies = Lens.field @"policies"
{-# DEPRECATED lpgsaePolicies "Use generic-lens or generic-optics with 'policies' instead." #-}

-- | The namespace of the service that was accessed.
--
-- To learn the service namespace of a service, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html Actions, Resources, and Condition Keys for AWS Services> in the /IAM User Guide/ . Choose the name of the service to view details for that service. In the first paragraph, find the service prefix. For example, @(service prefix: a4b)@ . For more information about service namespaces, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'serviceNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpgsaeServiceNamespace :: Lens.Lens' ListPoliciesGrantingServiceAccessEntry (Core.Maybe Types.ServiceNamespace)
lpgsaeServiceNamespace = Lens.field @"serviceNamespace"
{-# DEPRECATED lpgsaeServiceNamespace "Use generic-lens or generic-optics with 'serviceNamespace' instead." #-}

instance Core.FromXML ListPoliciesGrantingServiceAccessEntry where
  parseXML x =
    ListPoliciesGrantingServiceAccessEntry'
      Core.<$> (x Core..@? "Policies" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "ServiceNamespace")
