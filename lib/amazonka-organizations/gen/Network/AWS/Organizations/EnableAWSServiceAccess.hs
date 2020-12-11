{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.EnableAWSServiceAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the integration of an AWS service (the service that is specified by @ServicePrincipal@ ) with AWS Organizations. When you enable integration, you allow the specified service to create a <http://docs.aws.amazon.com/IAM/latest/UserGuide/using-service-linked-roles.html service-linked role> in all the accounts in your organization. This allows the service to perform operations on your behalf in your organization and its accounts.
--
-- /Important:/ We recommend that you enable integration between AWS Organizations and the specified AWS service by using the console or commands that are provided by the specified service. Doing so ensures that the service is aware that it can create the resources that are required for the integration. How the service creates those resources in the organization's accounts depends on that service. For more information, see the documentation for the other AWS service.
-- For more information about enabling services to integrate with AWS Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html Integrating AWS Organizations with Other AWS Services> in the /AWS Organizations User Guide./
-- This operation can be called only from the organization's management account and only if the organization has <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html enabled all features> .
module Network.AWS.Organizations.EnableAWSServiceAccess
  ( -- * Creating a request
    EnableAWSServiceAccess (..),
    mkEnableAWSServiceAccess,

    -- ** Request lenses
    easaServicePrincipal,

    -- * Destructuring the response
    EnableAWSServiceAccessResponse (..),
    mkEnableAWSServiceAccessResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEnableAWSServiceAccess' smart constructor.
newtype EnableAWSServiceAccess = EnableAWSServiceAccess'
  { servicePrincipal ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableAWSServiceAccess' with the minimum fields required to make a request.
--
-- * 'servicePrincipal' - The service principal name of the AWS service for which you want to enable integration with your organization. This is typically in the form of a URL, such as @/service-abbreviation/ .amazonaws.com@ .
mkEnableAWSServiceAccess ::
  -- | 'servicePrincipal'
  Lude.Text ->
  EnableAWSServiceAccess
mkEnableAWSServiceAccess pServicePrincipal_ =
  EnableAWSServiceAccess' {servicePrincipal = pServicePrincipal_}

-- | The service principal name of the AWS service for which you want to enable integration with your organization. This is typically in the form of a URL, such as @/service-abbreviation/ .amazonaws.com@ .
--
-- /Note:/ Consider using 'servicePrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
easaServicePrincipal :: Lens.Lens' EnableAWSServiceAccess Lude.Text
easaServicePrincipal = Lens.lens (servicePrincipal :: EnableAWSServiceAccess -> Lude.Text) (\s a -> s {servicePrincipal = a} :: EnableAWSServiceAccess)
{-# DEPRECATED easaServicePrincipal "Use generic-lens or generic-optics with 'servicePrincipal' instead." #-}

instance Lude.AWSRequest EnableAWSServiceAccess where
  type Rs EnableAWSServiceAccess = EnableAWSServiceAccessResponse
  request = Req.postJSON organizationsService
  response = Res.receiveNull EnableAWSServiceAccessResponse'

instance Lude.ToHeaders EnableAWSServiceAccess where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.EnableAWSServiceAccess" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON EnableAWSServiceAccess where
  toJSON EnableAWSServiceAccess' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ServicePrincipal" Lude..= servicePrincipal)]
      )

instance Lude.ToPath EnableAWSServiceAccess where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableAWSServiceAccess where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkEnableAWSServiceAccessResponse' smart constructor.
data EnableAWSServiceAccessResponse = EnableAWSServiceAccessResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableAWSServiceAccessResponse' with the minimum fields required to make a request.
mkEnableAWSServiceAccessResponse ::
  EnableAWSServiceAccessResponse
mkEnableAWSServiceAccessResponse = EnableAWSServiceAccessResponse'
