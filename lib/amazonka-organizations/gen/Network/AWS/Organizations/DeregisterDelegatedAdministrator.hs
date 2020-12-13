{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DeregisterDelegatedAdministrator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified member AWS account as a delegated administrator for the specified AWS service.
--
-- /Important:/ Deregistering a delegated administrator can have unintended impacts on the functionality of the enabled AWS service. See the documentation for the enabled service before you deregister a delegated administrator so that you understand any potential impacts.
-- You can run this action only for AWS services that support this feature. For a current list of services that support it, see the column /Supports Delegated Administrator/ in the table at <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrated-services-list.html AWS Services that you can use with AWS Organizations> in the /AWS Organizations User Guide./
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.DeregisterDelegatedAdministrator
  ( -- * Creating a request
    DeregisterDelegatedAdministrator (..),
    mkDeregisterDelegatedAdministrator,

    -- ** Request lenses
    ddaServicePrincipal,
    ddaAccountId,

    -- * Destructuring the response
    DeregisterDelegatedAdministratorResponse (..),
    mkDeregisterDelegatedAdministratorResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeregisterDelegatedAdministrator' smart constructor.
data DeregisterDelegatedAdministrator = DeregisterDelegatedAdministrator'
  { -- | The service principal name of an AWS service for which the account is a delegated administrator.
    --
    -- Delegated administrator privileges are revoked for only the specified AWS service from the member account. If the specified service is the only service for which the member account is a delegated administrator, the operation also revokes Organizations read action permissions.
    servicePrincipal :: Lude.Text,
    -- | The account ID number of the member account in the organization that you want to deregister as a delegated administrator.
    accountId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterDelegatedAdministrator' with the minimum fields required to make a request.
--
-- * 'servicePrincipal' - The service principal name of an AWS service for which the account is a delegated administrator.
--
-- Delegated administrator privileges are revoked for only the specified AWS service from the member account. If the specified service is the only service for which the member account is a delegated administrator, the operation also revokes Organizations read action permissions.
-- * 'accountId' - The account ID number of the member account in the organization that you want to deregister as a delegated administrator.
mkDeregisterDelegatedAdministrator ::
  -- | 'servicePrincipal'
  Lude.Text ->
  -- | 'accountId'
  Lude.Text ->
  DeregisterDelegatedAdministrator
mkDeregisterDelegatedAdministrator pServicePrincipal_ pAccountId_ =
  DeregisterDelegatedAdministrator'
    { servicePrincipal =
        pServicePrincipal_,
      accountId = pAccountId_
    }

-- | The service principal name of an AWS service for which the account is a delegated administrator.
--
-- Delegated administrator privileges are revoked for only the specified AWS service from the member account. If the specified service is the only service for which the member account is a delegated administrator, the operation also revokes Organizations read action permissions.
--
-- /Note:/ Consider using 'servicePrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddaServicePrincipal :: Lens.Lens' DeregisterDelegatedAdministrator Lude.Text
ddaServicePrincipal = Lens.lens (servicePrincipal :: DeregisterDelegatedAdministrator -> Lude.Text) (\s a -> s {servicePrincipal = a} :: DeregisterDelegatedAdministrator)
{-# DEPRECATED ddaServicePrincipal "Use generic-lens or generic-optics with 'servicePrincipal' instead." #-}

-- | The account ID number of the member account in the organization that you want to deregister as a delegated administrator.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddaAccountId :: Lens.Lens' DeregisterDelegatedAdministrator Lude.Text
ddaAccountId = Lens.lens (accountId :: DeregisterDelegatedAdministrator -> Lude.Text) (\s a -> s {accountId = a} :: DeregisterDelegatedAdministrator)
{-# DEPRECATED ddaAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Lude.AWSRequest DeregisterDelegatedAdministrator where
  type
    Rs DeregisterDelegatedAdministrator =
      DeregisterDelegatedAdministratorResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveNull DeregisterDelegatedAdministratorResponse'

instance Lude.ToHeaders DeregisterDelegatedAdministrator where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.DeregisterDelegatedAdministrator" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeregisterDelegatedAdministrator where
  toJSON DeregisterDelegatedAdministrator' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ServicePrincipal" Lude..= servicePrincipal),
            Lude.Just ("AccountId" Lude..= accountId)
          ]
      )

instance Lude.ToPath DeregisterDelegatedAdministrator where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterDelegatedAdministrator where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeregisterDelegatedAdministratorResponse' smart constructor.
data DeregisterDelegatedAdministratorResponse = DeregisterDelegatedAdministratorResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterDelegatedAdministratorResponse' with the minimum fields required to make a request.
mkDeregisterDelegatedAdministratorResponse ::
  DeregisterDelegatedAdministratorResponse
mkDeregisterDelegatedAdministratorResponse =
  DeregisterDelegatedAdministratorResponse'
