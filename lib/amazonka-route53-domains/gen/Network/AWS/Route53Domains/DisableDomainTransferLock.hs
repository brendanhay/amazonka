{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.DisableDomainTransferLock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation removes the transfer lock on the domain (specifically the @clientTransferProhibited@ status) to allow domain transfers. We recommend you refrain from performing this action unless you intend to transfer the domain to a different registrar. Successful submission returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.
module Network.AWS.Route53Domains.DisableDomainTransferLock
  ( -- * Creating a request
    DisableDomainTransferLock (..),
    mkDisableDomainTransferLock,

    -- ** Request lenses
    ddtlDomainName,

    -- * Destructuring the response
    DisableDomainTransferLockResponse (..),
    mkDisableDomainTransferLockResponse,

    -- ** Response lenses
    ddtlrsOperationId,
    ddtlrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | The DisableDomainTransferLock request includes the following element.
--
-- /See:/ 'mkDisableDomainTransferLock' smart constructor.
newtype DisableDomainTransferLock = DisableDomainTransferLock'
  { -- | The name of the domain that you want to remove the transfer lock for.
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableDomainTransferLock' with the minimum fields required to make a request.
--
-- * 'domainName' - The name of the domain that you want to remove the transfer lock for.
mkDisableDomainTransferLock ::
  -- | 'domainName'
  Lude.Text ->
  DisableDomainTransferLock
mkDisableDomainTransferLock pDomainName_ =
  DisableDomainTransferLock' {domainName = pDomainName_}

-- | The name of the domain that you want to remove the transfer lock for.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtlDomainName :: Lens.Lens' DisableDomainTransferLock Lude.Text
ddtlDomainName = Lens.lens (domainName :: DisableDomainTransferLock -> Lude.Text) (\s a -> s {domainName = a} :: DisableDomainTransferLock)
{-# DEPRECATED ddtlDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest DisableDomainTransferLock where
  type
    Rs DisableDomainTransferLock =
      DisableDomainTransferLockResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DisableDomainTransferLockResponse'
            Lude.<$> (x Lude..:> "OperationId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisableDomainTransferLock where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53Domains_v20140515.DisableDomainTransferLock" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisableDomainTransferLock where
  toJSON DisableDomainTransferLock' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("DomainName" Lude..= domainName)])

instance Lude.ToPath DisableDomainTransferLock where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableDomainTransferLock where
  toQuery = Lude.const Lude.mempty

-- | The DisableDomainTransferLock response includes the following element.
--
-- /See:/ 'mkDisableDomainTransferLockResponse' smart constructor.
data DisableDomainTransferLockResponse = DisableDomainTransferLockResponse'
  { -- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
    operationId :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableDomainTransferLockResponse' with the minimum fields required to make a request.
--
-- * 'operationId' - Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
-- * 'responseStatus' - The response status code.
mkDisableDomainTransferLockResponse ::
  -- | 'operationId'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  DisableDomainTransferLockResponse
mkDisableDomainTransferLockResponse pOperationId_ pResponseStatus_ =
  DisableDomainTransferLockResponse'
    { operationId = pOperationId_,
      responseStatus = pResponseStatus_
    }

-- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtlrsOperationId :: Lens.Lens' DisableDomainTransferLockResponse Lude.Text
ddtlrsOperationId = Lens.lens (operationId :: DisableDomainTransferLockResponse -> Lude.Text) (\s a -> s {operationId = a} :: DisableDomainTransferLockResponse)
{-# DEPRECATED ddtlrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddtlrsResponseStatus :: Lens.Lens' DisableDomainTransferLockResponse Lude.Int
ddtlrsResponseStatus = Lens.lens (responseStatus :: DisableDomainTransferLockResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisableDomainTransferLockResponse)
{-# DEPRECATED ddtlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
