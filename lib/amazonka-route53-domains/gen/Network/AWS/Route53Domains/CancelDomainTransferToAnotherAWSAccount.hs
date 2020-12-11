{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.CancelDomainTransferToAnotherAWSAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the transfer of a domain from the current AWS account to another AWS account. You initiate a transfer between AWS accounts using <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> .
--
-- /Important:/ You must cancel the transfer before the other AWS account accepts the transfer using <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_AcceptDomainTransferFromAnotherAwsAccount.html AcceptDomainTransferFromAnotherAwsAccount> .
-- Use either <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ListOperations.html ListOperations> or <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> to determine whether the operation succeeded. <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> provides additional information, for example, @Domain Transfer from Aws Account 111122223333 has been cancelled@ .
module Network.AWS.Route53Domains.CancelDomainTransferToAnotherAWSAccount
  ( -- * Creating a request
    CancelDomainTransferToAnotherAWSAccount (..),
    mkCancelDomainTransferToAnotherAWSAccount,

    -- ** Request lenses
    cdttaaaDomainName,

    -- * Destructuring the response
    CancelDomainTransferToAnotherAWSAccountResponse (..),
    mkCancelDomainTransferToAnotherAWSAccountResponse,

    -- ** Response lenses
    cdttaaarsOperationId,
    cdttaaarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | The CancelDomainTransferToAnotherAwsAccount request includes the following element.
--
-- /See:/ 'mkCancelDomainTransferToAnotherAWSAccount' smart constructor.
newtype CancelDomainTransferToAnotherAWSAccount = CancelDomainTransferToAnotherAWSAccount'
  { domainName ::
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

-- | Creates a value of 'CancelDomainTransferToAnotherAWSAccount' with the minimum fields required to make a request.
--
-- * 'domainName' - The name of the domain for which you want to cancel the transfer to another AWS account.
mkCancelDomainTransferToAnotherAWSAccount ::
  -- | 'domainName'
  Lude.Text ->
  CancelDomainTransferToAnotherAWSAccount
mkCancelDomainTransferToAnotherAWSAccount pDomainName_ =
  CancelDomainTransferToAnotherAWSAccount'
    { domainName =
        pDomainName_
    }

-- | The name of the domain for which you want to cancel the transfer to another AWS account.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdttaaaDomainName :: Lens.Lens' CancelDomainTransferToAnotherAWSAccount Lude.Text
cdttaaaDomainName = Lens.lens (domainName :: CancelDomainTransferToAnotherAWSAccount -> Lude.Text) (\s a -> s {domainName = a} :: CancelDomainTransferToAnotherAWSAccount)
{-# DEPRECATED cdttaaaDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest CancelDomainTransferToAnotherAWSAccount where
  type
    Rs CancelDomainTransferToAnotherAWSAccount =
      CancelDomainTransferToAnotherAWSAccountResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CancelDomainTransferToAnotherAWSAccountResponse'
            Lude.<$> (x Lude..?> "OperationId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelDomainTransferToAnotherAWSAccount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53Domains_v20140515.CancelDomainTransferToAnotherAwsAccount" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CancelDomainTransferToAnotherAWSAccount where
  toJSON CancelDomainTransferToAnotherAWSAccount' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("DomainName" Lude..= domainName)])

instance Lude.ToPath CancelDomainTransferToAnotherAWSAccount where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelDomainTransferToAnotherAWSAccount where
  toQuery = Lude.const Lude.mempty

-- | The @CancelDomainTransferToAnotherAwsAccount@ response includes the following element.
--
-- /See:/ 'mkCancelDomainTransferToAnotherAWSAccountResponse' smart constructor.
data CancelDomainTransferToAnotherAWSAccountResponse = CancelDomainTransferToAnotherAWSAccountResponse'
  { operationId ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'CancelDomainTransferToAnotherAWSAccountResponse' with the minimum fields required to make a request.
--
-- * 'operationId' - The identifier that @TransferDomainToAnotherAwsAccount@ returned to track the progress of the request. Because the transfer request was canceled, the value is no longer valid, and you can't use @GetOperationDetail@ to query the operation status.
-- * 'responseStatus' - The response status code.
mkCancelDomainTransferToAnotherAWSAccountResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelDomainTransferToAnotherAWSAccountResponse
mkCancelDomainTransferToAnotherAWSAccountResponse pResponseStatus_ =
  CancelDomainTransferToAnotherAWSAccountResponse'
    { operationId =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier that @TransferDomainToAnotherAwsAccount@ returned to track the progress of the request. Because the transfer request was canceled, the value is no longer valid, and you can't use @GetOperationDetail@ to query the operation status.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdttaaarsOperationId :: Lens.Lens' CancelDomainTransferToAnotherAWSAccountResponse (Lude.Maybe Lude.Text)
cdttaaarsOperationId = Lens.lens (operationId :: CancelDomainTransferToAnotherAWSAccountResponse -> Lude.Maybe Lude.Text) (\s a -> s {operationId = a} :: CancelDomainTransferToAnotherAWSAccountResponse)
{-# DEPRECATED cdttaaarsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdttaaarsResponseStatus :: Lens.Lens' CancelDomainTransferToAnotherAWSAccountResponse Lude.Int
cdttaaarsResponseStatus = Lens.lens (responseStatus :: CancelDomainTransferToAnotherAWSAccountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelDomainTransferToAnotherAWSAccountResponse)
{-# DEPRECATED cdttaaarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
