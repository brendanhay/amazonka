{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.EnableDomainTransferLock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation sets the transfer lock on the domain (specifically the @clientTransferProhibited@ status) to prevent domain transfers. Successful submission returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.
module Network.AWS.Route53Domains.EnableDomainTransferLock
  ( -- * Creating a request
    EnableDomainTransferLock (..),
    mkEnableDomainTransferLock,

    -- ** Request lenses
    edtlDomainName,

    -- * Destructuring the response
    EnableDomainTransferLockResponse (..),
    mkEnableDomainTransferLockResponse,

    -- ** Response lenses
    edtlrsOperationId,
    edtlrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | A request to set the transfer lock for the specified domain.
--
-- /See:/ 'mkEnableDomainTransferLock' smart constructor.
newtype EnableDomainTransferLock = EnableDomainTransferLock'
  { -- | The name of the domain that you want to set the transfer lock for.
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableDomainTransferLock' with the minimum fields required to make a request.
--
-- * 'domainName' - The name of the domain that you want to set the transfer lock for.
mkEnableDomainTransferLock ::
  -- | 'domainName'
  Lude.Text ->
  EnableDomainTransferLock
mkEnableDomainTransferLock pDomainName_ =
  EnableDomainTransferLock' {domainName = pDomainName_}

-- | The name of the domain that you want to set the transfer lock for.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edtlDomainName :: Lens.Lens' EnableDomainTransferLock Lude.Text
edtlDomainName = Lens.lens (domainName :: EnableDomainTransferLock -> Lude.Text) (\s a -> s {domainName = a} :: EnableDomainTransferLock)
{-# DEPRECATED edtlDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest EnableDomainTransferLock where
  type Rs EnableDomainTransferLock = EnableDomainTransferLockResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          EnableDomainTransferLockResponse'
            Lude.<$> (x Lude..:> "OperationId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders EnableDomainTransferLock where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53Domains_v20140515.EnableDomainTransferLock" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON EnableDomainTransferLock where
  toJSON EnableDomainTransferLock' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("DomainName" Lude..= domainName)])

instance Lude.ToPath EnableDomainTransferLock where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableDomainTransferLock where
  toQuery = Lude.const Lude.mempty

-- | The EnableDomainTransferLock response includes the following elements.
--
-- /See:/ 'mkEnableDomainTransferLockResponse' smart constructor.
data EnableDomainTransferLockResponse = EnableDomainTransferLockResponse'
  { -- | Identifier for tracking the progress of the request. To use this ID to query the operation status, use GetOperationDetail.
    operationId :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableDomainTransferLockResponse' with the minimum fields required to make a request.
--
-- * 'operationId' - Identifier for tracking the progress of the request. To use this ID to query the operation status, use GetOperationDetail.
-- * 'responseStatus' - The response status code.
mkEnableDomainTransferLockResponse ::
  -- | 'operationId'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  EnableDomainTransferLockResponse
mkEnableDomainTransferLockResponse pOperationId_ pResponseStatus_ =
  EnableDomainTransferLockResponse'
    { operationId = pOperationId_,
      responseStatus = pResponseStatus_
    }

-- | Identifier for tracking the progress of the request. To use this ID to query the operation status, use GetOperationDetail.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edtlrsOperationId :: Lens.Lens' EnableDomainTransferLockResponse Lude.Text
edtlrsOperationId = Lens.lens (operationId :: EnableDomainTransferLockResponse -> Lude.Text) (\s a -> s {operationId = a} :: EnableDomainTransferLockResponse)
{-# DEPRECATED edtlrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edtlrsResponseStatus :: Lens.Lens' EnableDomainTransferLockResponse Lude.Int
edtlrsResponseStatus = Lens.lens (responseStatus :: EnableDomainTransferLockResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EnableDomainTransferLockResponse)
{-# DEPRECATED edtlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
