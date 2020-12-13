{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.GetOperationDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the current status of an operation that is not completed.
module Network.AWS.Route53Domains.GetOperationDetail
  ( -- * Creating a request
    GetOperationDetail (..),
    mkGetOperationDetail,

    -- ** Request lenses
    godOperationId,

    -- * Destructuring the response
    GetOperationDetailResponse (..),
    mkGetOperationDetailResponse,

    -- ** Response lenses
    godrsStatus,
    godrsSubmittedDate,
    godrsDomainName,
    godrsOperationId,
    godrsType,
    godrsMessage,
    godrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | The <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> request includes the following element.
--
-- /See:/ 'mkGetOperationDetail' smart constructor.
newtype GetOperationDetail = GetOperationDetail'
  { -- | The identifier for the operation for which you want to get the status. Route 53 returned the identifier in the response to the original request.
    operationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOperationDetail' with the minimum fields required to make a request.
--
-- * 'operationId' - The identifier for the operation for which you want to get the status. Route 53 returned the identifier in the response to the original request.
mkGetOperationDetail ::
  -- | 'operationId'
  Lude.Text ->
  GetOperationDetail
mkGetOperationDetail pOperationId_ =
  GetOperationDetail' {operationId = pOperationId_}

-- | The identifier for the operation for which you want to get the status. Route 53 returned the identifier in the response to the original request.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
godOperationId :: Lens.Lens' GetOperationDetail Lude.Text
godOperationId = Lens.lens (operationId :: GetOperationDetail -> Lude.Text) (\s a -> s {operationId = a} :: GetOperationDetail)
{-# DEPRECATED godOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

instance Lude.AWSRequest GetOperationDetail where
  type Rs GetOperationDetail = GetOperationDetailResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetOperationDetailResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "SubmittedDate")
            Lude.<*> (x Lude..?> "DomainName")
            Lude.<*> (x Lude..?> "OperationId")
            Lude.<*> (x Lude..?> "Type")
            Lude.<*> (x Lude..?> "Message")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetOperationDetail where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Route53Domains_v20140515.GetOperationDetail" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetOperationDetail where
  toJSON GetOperationDetail' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("OperationId" Lude..= operationId)])

instance Lude.ToPath GetOperationDetail where
  toPath = Lude.const "/"

instance Lude.ToQuery GetOperationDetail where
  toQuery = Lude.const Lude.mempty

-- | The GetOperationDetail response includes the following elements.
--
-- /See:/ 'mkGetOperationDetailResponse' smart constructor.
data GetOperationDetailResponse = GetOperationDetailResponse'
  { -- | The current status of the requested operation in the system.
    status :: Lude.Maybe OperationStatus,
    -- | The date when the request was submitted.
    submittedDate :: Lude.Maybe Lude.Timestamp,
    -- | The name of a domain.
    domainName :: Lude.Maybe Lude.Text,
    -- | The identifier for the operation.
    operationId :: Lude.Maybe Lude.Text,
    -- | The type of operation that was requested.
    type' :: Lude.Maybe OperationType,
    -- | Detailed information on the status including possible errors.
    message :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetOperationDetailResponse' with the minimum fields required to make a request.
--
-- * 'status' - The current status of the requested operation in the system.
-- * 'submittedDate' - The date when the request was submitted.
-- * 'domainName' - The name of a domain.
-- * 'operationId' - The identifier for the operation.
-- * 'type'' - The type of operation that was requested.
-- * 'message' - Detailed information on the status including possible errors.
-- * 'responseStatus' - The response status code.
mkGetOperationDetailResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetOperationDetailResponse
mkGetOperationDetailResponse pResponseStatus_ =
  GetOperationDetailResponse'
    { status = Lude.Nothing,
      submittedDate = Lude.Nothing,
      domainName = Lude.Nothing,
      operationId = Lude.Nothing,
      type' = Lude.Nothing,
      message = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current status of the requested operation in the system.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
godrsStatus :: Lens.Lens' GetOperationDetailResponse (Lude.Maybe OperationStatus)
godrsStatus = Lens.lens (status :: GetOperationDetailResponse -> Lude.Maybe OperationStatus) (\s a -> s {status = a} :: GetOperationDetailResponse)
{-# DEPRECATED godrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date when the request was submitted.
--
-- /Note:/ Consider using 'submittedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
godrsSubmittedDate :: Lens.Lens' GetOperationDetailResponse (Lude.Maybe Lude.Timestamp)
godrsSubmittedDate = Lens.lens (submittedDate :: GetOperationDetailResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {submittedDate = a} :: GetOperationDetailResponse)
{-# DEPRECATED godrsSubmittedDate "Use generic-lens or generic-optics with 'submittedDate' instead." #-}

-- | The name of a domain.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
godrsDomainName :: Lens.Lens' GetOperationDetailResponse (Lude.Maybe Lude.Text)
godrsDomainName = Lens.lens (domainName :: GetOperationDetailResponse -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: GetOperationDetailResponse)
{-# DEPRECATED godrsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The identifier for the operation.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
godrsOperationId :: Lens.Lens' GetOperationDetailResponse (Lude.Maybe Lude.Text)
godrsOperationId = Lens.lens (operationId :: GetOperationDetailResponse -> Lude.Maybe Lude.Text) (\s a -> s {operationId = a} :: GetOperationDetailResponse)
{-# DEPRECATED godrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The type of operation that was requested.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
godrsType :: Lens.Lens' GetOperationDetailResponse (Lude.Maybe OperationType)
godrsType = Lens.lens (type' :: GetOperationDetailResponse -> Lude.Maybe OperationType) (\s a -> s {type' = a} :: GetOperationDetailResponse)
{-# DEPRECATED godrsType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Detailed information on the status including possible errors.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
godrsMessage :: Lens.Lens' GetOperationDetailResponse (Lude.Maybe Lude.Text)
godrsMessage = Lens.lens (message :: GetOperationDetailResponse -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: GetOperationDetailResponse)
{-# DEPRECATED godrsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
godrsResponseStatus :: Lens.Lens' GetOperationDetailResponse Lude.Int
godrsResponseStatus = Lens.lens (responseStatus :: GetOperationDetailResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetOperationDetailResponse)
{-# DEPRECATED godrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
