{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.RejectDomainTransferFromAnotherAWSAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects the transfer of a domain from another AWS account to the current AWS account. You initiate a transfer between AWS accounts using <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> .
--
-- Use either <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ListOperations.html ListOperations> or <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> to determine whether the operation succeeded. <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> provides additional information, for example, @Domain Transfer from Aws Account 111122223333 has been cancelled@ .
module Network.AWS.Route53Domains.RejectDomainTransferFromAnotherAWSAccount
  ( -- * Creating a request
    RejectDomainTransferFromAnotherAWSAccount (..),
    mkRejectDomainTransferFromAnotherAWSAccount,

    -- ** Request lenses
    rdtfaaaDomainName,

    -- * Destructuring the response
    RejectDomainTransferFromAnotherAWSAccountResponse (..),
    mkRejectDomainTransferFromAnotherAWSAccountResponse,

    -- ** Response lenses
    rdtfaaarsOperationId,
    rdtfaaarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | The RejectDomainTransferFromAnotherAwsAccount request includes the following element.
--
-- /See:/ 'mkRejectDomainTransferFromAnotherAWSAccount' smart constructor.
newtype RejectDomainTransferFromAnotherAWSAccount = RejectDomainTransferFromAnotherAWSAccount'
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

-- | Creates a value of 'RejectDomainTransferFromAnotherAWSAccount' with the minimum fields required to make a request.
--
-- * 'domainName' - The name of the domain that was specified when another AWS account submitted a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> request.
mkRejectDomainTransferFromAnotherAWSAccount ::
  -- | 'domainName'
  Lude.Text ->
  RejectDomainTransferFromAnotherAWSAccount
mkRejectDomainTransferFromAnotherAWSAccount pDomainName_ =
  RejectDomainTransferFromAnotherAWSAccount'
    { domainName =
        pDomainName_
    }

-- | The name of the domain that was specified when another AWS account submitted a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> request.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdtfaaaDomainName :: Lens.Lens' RejectDomainTransferFromAnotherAWSAccount Lude.Text
rdtfaaaDomainName = Lens.lens (domainName :: RejectDomainTransferFromAnotherAWSAccount -> Lude.Text) (\s a -> s {domainName = a} :: RejectDomainTransferFromAnotherAWSAccount)
{-# DEPRECATED rdtfaaaDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest RejectDomainTransferFromAnotherAWSAccount where
  type
    Rs RejectDomainTransferFromAnotherAWSAccount =
      RejectDomainTransferFromAnotherAWSAccountResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          RejectDomainTransferFromAnotherAWSAccountResponse'
            Lude.<$> (x Lude..?> "OperationId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RejectDomainTransferFromAnotherAWSAccount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53Domains_v20140515.RejectDomainTransferFromAnotherAwsAccount" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RejectDomainTransferFromAnotherAWSAccount where
  toJSON RejectDomainTransferFromAnotherAWSAccount' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("DomainName" Lude..= domainName)])

instance Lude.ToPath RejectDomainTransferFromAnotherAWSAccount where
  toPath = Lude.const "/"

instance Lude.ToQuery RejectDomainTransferFromAnotherAWSAccount where
  toQuery = Lude.const Lude.mempty

-- | The RejectDomainTransferFromAnotherAwsAccount response includes the following element.
--
-- /See:/ 'mkRejectDomainTransferFromAnotherAWSAccountResponse' smart constructor.
data RejectDomainTransferFromAnotherAWSAccountResponse = RejectDomainTransferFromAnotherAWSAccountResponse'
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

-- | Creates a value of 'RejectDomainTransferFromAnotherAWSAccountResponse' with the minimum fields required to make a request.
--
-- * 'operationId' - The identifier that @TransferDomainToAnotherAwsAccount@ returned to track the progress of the request. Because the transfer request was rejected, the value is no longer valid, and you can't use @GetOperationDetail@ to query the operation status.
-- * 'responseStatus' - The response status code.
mkRejectDomainTransferFromAnotherAWSAccountResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RejectDomainTransferFromAnotherAWSAccountResponse
mkRejectDomainTransferFromAnotherAWSAccountResponse
  pResponseStatus_ =
    RejectDomainTransferFromAnotherAWSAccountResponse'
      { operationId =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | The identifier that @TransferDomainToAnotherAwsAccount@ returned to track the progress of the request. Because the transfer request was rejected, the value is no longer valid, and you can't use @GetOperationDetail@ to query the operation status.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdtfaaarsOperationId :: Lens.Lens' RejectDomainTransferFromAnotherAWSAccountResponse (Lude.Maybe Lude.Text)
rdtfaaarsOperationId = Lens.lens (operationId :: RejectDomainTransferFromAnotherAWSAccountResponse -> Lude.Maybe Lude.Text) (\s a -> s {operationId = a} :: RejectDomainTransferFromAnotherAWSAccountResponse)
{-# DEPRECATED rdtfaaarsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdtfaaarsResponseStatus :: Lens.Lens' RejectDomainTransferFromAnotherAWSAccountResponse Lude.Int
rdtfaaarsResponseStatus = Lens.lens (responseStatus :: RejectDomainTransferFromAnotherAWSAccountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RejectDomainTransferFromAnotherAWSAccountResponse)
{-# DEPRECATED rdtfaaarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
