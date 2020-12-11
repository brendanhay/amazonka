{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.UpdateDomainContact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation updates the contact information for a particular domain. You must specify information for at least one contact: registrant, administrator, or technical.
--
-- If the update is successful, this method returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.
module Network.AWS.Route53Domains.UpdateDomainContact
  ( -- * Creating a request
    UpdateDomainContact (..),
    mkUpdateDomainContact,

    -- ** Request lenses
    udcRegistrantContact,
    udcAdminContact,
    udcTechContact,
    udcDomainName,

    -- * Destructuring the response
    UpdateDomainContactResponse (..),
    mkUpdateDomainContactResponse,

    -- ** Response lenses
    udcrsResponseStatus,
    udcrsOperationId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | The UpdateDomainContact request includes the following elements.
--
-- /See:/ 'mkUpdateDomainContact' smart constructor.
data UpdateDomainContact = UpdateDomainContact'
  { registrantContact ::
      Lude.Maybe ContactDetail,
    adminContact :: Lude.Maybe ContactDetail,
    techContact :: Lude.Maybe ContactDetail,
    domainName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDomainContact' with the minimum fields required to make a request.
--
-- * 'adminContact' - Provides detailed contact information.
-- * 'domainName' - The name of the domain that you want to update contact information for.
-- * 'registrantContact' - Provides detailed contact information.
-- * 'techContact' - Provides detailed contact information.
mkUpdateDomainContact ::
  -- | 'domainName'
  Lude.Text ->
  UpdateDomainContact
mkUpdateDomainContact pDomainName_ =
  UpdateDomainContact'
    { registrantContact = Lude.Nothing,
      adminContact = Lude.Nothing,
      techContact = Lude.Nothing,
      domainName = pDomainName_
    }

-- | Provides detailed contact information.
--
-- /Note:/ Consider using 'registrantContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcRegistrantContact :: Lens.Lens' UpdateDomainContact (Lude.Maybe ContactDetail)
udcRegistrantContact = Lens.lens (registrantContact :: UpdateDomainContact -> Lude.Maybe ContactDetail) (\s a -> s {registrantContact = a} :: UpdateDomainContact)
{-# DEPRECATED udcRegistrantContact "Use generic-lens or generic-optics with 'registrantContact' instead." #-}

-- | Provides detailed contact information.
--
-- /Note:/ Consider using 'adminContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcAdminContact :: Lens.Lens' UpdateDomainContact (Lude.Maybe ContactDetail)
udcAdminContact = Lens.lens (adminContact :: UpdateDomainContact -> Lude.Maybe ContactDetail) (\s a -> s {adminContact = a} :: UpdateDomainContact)
{-# DEPRECATED udcAdminContact "Use generic-lens or generic-optics with 'adminContact' instead." #-}

-- | Provides detailed contact information.
--
-- /Note:/ Consider using 'techContact' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcTechContact :: Lens.Lens' UpdateDomainContact (Lude.Maybe ContactDetail)
udcTechContact = Lens.lens (techContact :: UpdateDomainContact -> Lude.Maybe ContactDetail) (\s a -> s {techContact = a} :: UpdateDomainContact)
{-# DEPRECATED udcTechContact "Use generic-lens or generic-optics with 'techContact' instead." #-}

-- | The name of the domain that you want to update contact information for.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcDomainName :: Lens.Lens' UpdateDomainContact Lude.Text
udcDomainName = Lens.lens (domainName :: UpdateDomainContact -> Lude.Text) (\s a -> s {domainName = a} :: UpdateDomainContact)
{-# DEPRECATED udcDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest UpdateDomainContact where
  type Rs UpdateDomainContact = UpdateDomainContactResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateDomainContactResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "OperationId")
      )

instance Lude.ToHeaders UpdateDomainContact where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53Domains_v20140515.UpdateDomainContact" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDomainContact where
  toJSON UpdateDomainContact' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RegistrantContact" Lude..=) Lude.<$> registrantContact,
            ("AdminContact" Lude..=) Lude.<$> adminContact,
            ("TechContact" Lude..=) Lude.<$> techContact,
            Lude.Just ("DomainName" Lude..= domainName)
          ]
      )

instance Lude.ToPath UpdateDomainContact where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateDomainContact where
  toQuery = Lude.const Lude.mempty

-- | The UpdateDomainContact response includes the following element.
--
-- /See:/ 'mkUpdateDomainContactResponse' smart constructor.
data UpdateDomainContactResponse = UpdateDomainContactResponse'
  { responseStatus ::
      Lude.Int,
    operationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDomainContactResponse' with the minimum fields required to make a request.
--
-- * 'operationId' - Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
-- * 'responseStatus' - The response status code.
mkUpdateDomainContactResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'operationId'
  Lude.Text ->
  UpdateDomainContactResponse
mkUpdateDomainContactResponse pResponseStatus_ pOperationId_ =
  UpdateDomainContactResponse'
    { responseStatus = pResponseStatus_,
      operationId = pOperationId_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcrsResponseStatus :: Lens.Lens' UpdateDomainContactResponse Lude.Int
udcrsResponseStatus = Lens.lens (responseStatus :: UpdateDomainContactResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDomainContactResponse)
{-# DEPRECATED udcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcrsOperationId :: Lens.Lens' UpdateDomainContactResponse Lude.Text
udcrsOperationId = Lens.lens (operationId :: UpdateDomainContactResponse -> Lude.Text) (\s a -> s {operationId = a} :: UpdateDomainContactResponse)
{-# DEPRECATED udcrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}
