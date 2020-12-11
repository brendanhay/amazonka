{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DisableAWSServiceAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the integration of an AWS service (the service that is specified by @ServicePrincipal@ ) with AWS Organizations. When you disable integration, the specified service no longer can create a <http://docs.aws.amazon.com/IAM/latest/UserGuide/using-service-linked-roles.html service-linked role> in /new/ accounts in your organization. This means the service can't perform operations on your behalf on any new accounts in your organization. The service can still perform operations in older accounts until the service completes its clean-up from AWS Organizations.
--
--
-- /Important:/ We recommend that you disable integration between AWS Organizations and the specified AWS service by using the console or commands that are provided by the specified service. Doing so ensures that the other service is aware that it can clean up any resources that are required only for the integration. How the service cleans up its resources in the organization's accounts depends on that service. For more information, see the documentation for the other AWS service.
-- After you perform the @DisableAWSServiceAccess@ operation, the specified service can no longer perform operations in your organization's accounts unless the operations are explicitly permitted by the IAM policies that are attached to your roles.
-- For more information about integrating other services with AWS Organizations, including the list of services that work with Organizations, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrate_services.html Integrating AWS Organizations with Other AWS Services> in the /AWS Organizations User Guide./
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.DisableAWSServiceAccess
  ( -- * Creating a request
    DisableAWSServiceAccess (..),
    mkDisableAWSServiceAccess,

    -- ** Request lenses
    dasaServicePrincipal,

    -- * Destructuring the response
    DisableAWSServiceAccessResponse (..),
    mkDisableAWSServiceAccessResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisableAWSServiceAccess' smart constructor.
newtype DisableAWSServiceAccess = DisableAWSServiceAccess'
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

-- | Creates a value of 'DisableAWSServiceAccess' with the minimum fields required to make a request.
--
-- * 'servicePrincipal' - The service principal name of the AWS service for which you want to disable integration with your organization. This is typically in the form of a URL, such as @/service-abbreviation/ .amazonaws.com@ .
mkDisableAWSServiceAccess ::
  -- | 'servicePrincipal'
  Lude.Text ->
  DisableAWSServiceAccess
mkDisableAWSServiceAccess pServicePrincipal_ =
  DisableAWSServiceAccess' {servicePrincipal = pServicePrincipal_}

-- | The service principal name of the AWS service for which you want to disable integration with your organization. This is typically in the form of a URL, such as @/service-abbreviation/ .amazonaws.com@ .
--
-- /Note:/ Consider using 'servicePrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasaServicePrincipal :: Lens.Lens' DisableAWSServiceAccess Lude.Text
dasaServicePrincipal = Lens.lens (servicePrincipal :: DisableAWSServiceAccess -> Lude.Text) (\s a -> s {servicePrincipal = a} :: DisableAWSServiceAccess)
{-# DEPRECATED dasaServicePrincipal "Use generic-lens or generic-optics with 'servicePrincipal' instead." #-}

instance Lude.AWSRequest DisableAWSServiceAccess where
  type Rs DisableAWSServiceAccess = DisableAWSServiceAccessResponse
  request = Req.postJSON organizationsService
  response = Res.receiveNull DisableAWSServiceAccessResponse'

instance Lude.ToHeaders DisableAWSServiceAccess where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.DisableAWSServiceAccess" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisableAWSServiceAccess where
  toJSON DisableAWSServiceAccess' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ServicePrincipal" Lude..= servicePrincipal)]
      )

instance Lude.ToPath DisableAWSServiceAccess where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableAWSServiceAccess where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisableAWSServiceAccessResponse' smart constructor.
data DisableAWSServiceAccessResponse = DisableAWSServiceAccessResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableAWSServiceAccessResponse' with the minimum fields required to make a request.
mkDisableAWSServiceAccessResponse ::
  DisableAWSServiceAccessResponse
mkDisableAWSServiceAccessResponse =
  DisableAWSServiceAccessResponse'
