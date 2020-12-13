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
    lpgsaeServiceNamespace,
    lpgsaePolicies,
  )
where

import Network.AWS.IAM.Types.PolicyGrantingServiceAccess
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains details about the permissions policies that are attached to the specified identity (user, group, or role).
--
-- This data type is used as a response element in the 'ListPoliciesGrantingServiceAccess' operation.
--
-- /See:/ 'mkListPoliciesGrantingServiceAccessEntry' smart constructor.
data ListPoliciesGrantingServiceAccessEntry = ListPoliciesGrantingServiceAccessEntry'
  { -- | The namespace of the service that was accessed.
    --
    -- To learn the service namespace of a service, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html Actions, Resources, and Condition Keys for AWS Services> in the /IAM User Guide/ . Choose the name of the service to view details for that service. In the first paragraph, find the service prefix. For example, @(service prefix: a4b)@ . For more information about service namespaces, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /AWS General Reference/ .
    serviceNamespace :: Lude.Maybe Lude.Text,
    -- | The @PoliciesGrantingServiceAccess@ object that contains details about the policy.
    policies :: Lude.Maybe [PolicyGrantingServiceAccess]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListPoliciesGrantingServiceAccessEntry' with the minimum fields required to make a request.
--
-- * 'serviceNamespace' - The namespace of the service that was accessed.
--
-- To learn the service namespace of a service, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html Actions, Resources, and Condition Keys for AWS Services> in the /IAM User Guide/ . Choose the name of the service to view details for that service. In the first paragraph, find the service prefix. For example, @(service prefix: a4b)@ . For more information about service namespaces, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /AWS General Reference/ .
-- * 'policies' - The @PoliciesGrantingServiceAccess@ object that contains details about the policy.
mkListPoliciesGrantingServiceAccessEntry ::
  ListPoliciesGrantingServiceAccessEntry
mkListPoliciesGrantingServiceAccessEntry =
  ListPoliciesGrantingServiceAccessEntry'
    { serviceNamespace =
        Lude.Nothing,
      policies = Lude.Nothing
    }

-- | The namespace of the service that was accessed.
--
-- To learn the service namespace of a service, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_actions-resources-contextkeys.html Actions, Resources, and Condition Keys for AWS Services> in the /IAM User Guide/ . Choose the name of the service to view details for that service. In the first paragraph, find the service prefix. For example, @(service prefix: a4b)@ . For more information about service namespaces, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#genref-aws-service-namespaces AWS Service Namespaces> in the /AWS General Reference/ .
--
-- /Note:/ Consider using 'serviceNamespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpgsaeServiceNamespace :: Lens.Lens' ListPoliciesGrantingServiceAccessEntry (Lude.Maybe Lude.Text)
lpgsaeServiceNamespace = Lens.lens (serviceNamespace :: ListPoliciesGrantingServiceAccessEntry -> Lude.Maybe Lude.Text) (\s a -> s {serviceNamespace = a} :: ListPoliciesGrantingServiceAccessEntry)
{-# DEPRECATED lpgsaeServiceNamespace "Use generic-lens or generic-optics with 'serviceNamespace' instead." #-}

-- | The @PoliciesGrantingServiceAccess@ object that contains details about the policy.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpgsaePolicies :: Lens.Lens' ListPoliciesGrantingServiceAccessEntry (Lude.Maybe [PolicyGrantingServiceAccess])
lpgsaePolicies = Lens.lens (policies :: ListPoliciesGrantingServiceAccessEntry -> Lude.Maybe [PolicyGrantingServiceAccess]) (\s a -> s {policies = a} :: ListPoliciesGrantingServiceAccessEntry)
{-# DEPRECATED lpgsaePolicies "Use generic-lens or generic-optics with 'policies' instead." #-}

instance Lude.FromXML ListPoliciesGrantingServiceAccessEntry where
  parseXML x =
    ListPoliciesGrantingServiceAccessEntry'
      Lude.<$> (x Lude..@? "ServiceNamespace")
      Lude.<*> ( x Lude..@? "Policies" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
