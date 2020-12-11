{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.RegisterDelegatedAdministrator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified member account to administer the Organizations features of the specified AWS service. It grants read-only access to AWS Organizations service data. The account still requires IAM permissions to access and administer the AWS service.
--
-- You can run this action only for AWS services that support this feature. For a current list of services that support it, see the column /Supports Delegated Administrator/ in the table at <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_integrated-services-list.html AWS Services that you can use with AWS Organizations> in the /AWS Organizations User Guide./
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.RegisterDelegatedAdministrator
  ( -- * Creating a request
    RegisterDelegatedAdministrator (..),
    mkRegisterDelegatedAdministrator,

    -- ** Request lenses
    rdaAccountId,
    rdaServicePrincipal,

    -- * Destructuring the response
    RegisterDelegatedAdministratorResponse (..),
    mkRegisterDelegatedAdministratorResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterDelegatedAdministrator' smart constructor.
data RegisterDelegatedAdministrator = RegisterDelegatedAdministrator'
  { accountId ::
      Lude.Text,
    servicePrincipal :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterDelegatedAdministrator' with the minimum fields required to make a request.
--
-- * 'accountId' - The account ID number of the member account in the organization to register as a delegated administrator.
-- * 'servicePrincipal' - The service principal of the AWS service for which you want to make the member account a delegated administrator.
mkRegisterDelegatedAdministrator ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'servicePrincipal'
  Lude.Text ->
  RegisterDelegatedAdministrator
mkRegisterDelegatedAdministrator pAccountId_ pServicePrincipal_ =
  RegisterDelegatedAdministrator'
    { accountId = pAccountId_,
      servicePrincipal = pServicePrincipal_
    }

-- | The account ID number of the member account in the organization to register as a delegated administrator.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdaAccountId :: Lens.Lens' RegisterDelegatedAdministrator Lude.Text
rdaAccountId = Lens.lens (accountId :: RegisterDelegatedAdministrator -> Lude.Text) (\s a -> s {accountId = a} :: RegisterDelegatedAdministrator)
{-# DEPRECATED rdaAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The service principal of the AWS service for which you want to make the member account a delegated administrator.
--
-- /Note:/ Consider using 'servicePrincipal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdaServicePrincipal :: Lens.Lens' RegisterDelegatedAdministrator Lude.Text
rdaServicePrincipal = Lens.lens (servicePrincipal :: RegisterDelegatedAdministrator -> Lude.Text) (\s a -> s {servicePrincipal = a} :: RegisterDelegatedAdministrator)
{-# DEPRECATED rdaServicePrincipal "Use generic-lens or generic-optics with 'servicePrincipal' instead." #-}

instance Lude.AWSRequest RegisterDelegatedAdministrator where
  type
    Rs RegisterDelegatedAdministrator =
      RegisterDelegatedAdministratorResponse
  request = Req.postJSON organizationsService
  response = Res.receiveNull RegisterDelegatedAdministratorResponse'

instance Lude.ToHeaders RegisterDelegatedAdministrator where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.RegisterDelegatedAdministrator" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RegisterDelegatedAdministrator where
  toJSON RegisterDelegatedAdministrator' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AccountId" Lude..= accountId),
            Lude.Just ("ServicePrincipal" Lude..= servicePrincipal)
          ]
      )

instance Lude.ToPath RegisterDelegatedAdministrator where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterDelegatedAdministrator where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRegisterDelegatedAdministratorResponse' smart constructor.
data RegisterDelegatedAdministratorResponse = RegisterDelegatedAdministratorResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterDelegatedAdministratorResponse' with the minimum fields required to make a request.
mkRegisterDelegatedAdministratorResponse ::
  RegisterDelegatedAdministratorResponse
mkRegisterDelegatedAdministratorResponse =
  RegisterDelegatedAdministratorResponse'
