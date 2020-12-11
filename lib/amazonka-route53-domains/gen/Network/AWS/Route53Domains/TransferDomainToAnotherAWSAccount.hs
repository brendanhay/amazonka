{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.TransferDomainToAnotherAWSAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Transfers a domain from the current AWS account to another AWS account. Note the following:
--
--
--     * The AWS account that you're transferring the domain to must accept the transfer. If the other account doesn't accept the transfer within 3 days, we cancel the transfer. See <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_AcceptDomainTransferFromAnotherAwsAccount.html AcceptDomainTransferFromAnotherAwsAccount> .
--
--
--     * You can cancel the transfer before the other account accepts it. See <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_CancelDomainTransferToAnotherAwsAccount.html CancelDomainTransferToAnotherAwsAccount> .
--
--
--     * The other account can reject the transfer. See <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_RejectDomainTransferFromAnotherAwsAccount.html RejectDomainTransferFromAnotherAwsAccount> .
--
--
-- /Important:/ When you transfer a domain from one AWS account to another, Route 53 doesn't transfer the hosted zone that is associated with the domain. DNS resolution isn't affected if the domain and the hosted zone are owned by separate accounts, so transferring the hosted zone is optional. For information about transferring the hosted zone to another AWS account, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/hosted-zones-migrating.html Migrating a Hosted Zone to a Different AWS Account> in the /Amazon Route 53 Developer Guide/ .
-- Use either <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ListOperations.html ListOperations> or <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> to determine whether the operation succeeded. <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> provides additional information, for example, @Domain Transfer from Aws Account 111122223333 has been cancelled@ .
module Network.AWS.Route53Domains.TransferDomainToAnotherAWSAccount
  ( -- * Creating a request
    TransferDomainToAnotherAWSAccount (..),
    mkTransferDomainToAnotherAWSAccount,

    -- ** Request lenses
    tdtaaaDomainName,
    tdtaaaAccountId,

    -- * Destructuring the response
    TransferDomainToAnotherAWSAccountResponse (..),
    mkTransferDomainToAnotherAWSAccountResponse,

    -- ** Response lenses
    tdtaaarsPassword,
    tdtaaarsOperationId,
    tdtaaarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | The TransferDomainToAnotherAwsAccount request includes the following elements.
--
-- /See:/ 'mkTransferDomainToAnotherAWSAccount' smart constructor.
data TransferDomainToAnotherAWSAccount = TransferDomainToAnotherAWSAccount'
  { domainName ::
      Lude.Text,
    accountId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransferDomainToAnotherAWSAccount' with the minimum fields required to make a request.
--
-- * 'accountId' - The account ID of the AWS account that you want to transfer the domain to, for example, @111122223333@ .
-- * 'domainName' - The name of the domain that you want to transfer from the current AWS account to another account.
mkTransferDomainToAnotherAWSAccount ::
  -- | 'domainName'
  Lude.Text ->
  -- | 'accountId'
  Lude.Text ->
  TransferDomainToAnotherAWSAccount
mkTransferDomainToAnotherAWSAccount pDomainName_ pAccountId_ =
  TransferDomainToAnotherAWSAccount'
    { domainName = pDomainName_,
      accountId = pAccountId_
    }

-- | The name of the domain that you want to transfer from the current AWS account to another account.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdtaaaDomainName :: Lens.Lens' TransferDomainToAnotherAWSAccount Lude.Text
tdtaaaDomainName = Lens.lens (domainName :: TransferDomainToAnotherAWSAccount -> Lude.Text) (\s a -> s {domainName = a} :: TransferDomainToAnotherAWSAccount)
{-# DEPRECATED tdtaaaDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The account ID of the AWS account that you want to transfer the domain to, for example, @111122223333@ .
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdtaaaAccountId :: Lens.Lens' TransferDomainToAnotherAWSAccount Lude.Text
tdtaaaAccountId = Lens.lens (accountId :: TransferDomainToAnotherAWSAccount -> Lude.Text) (\s a -> s {accountId = a} :: TransferDomainToAnotherAWSAccount)
{-# DEPRECATED tdtaaaAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Lude.AWSRequest TransferDomainToAnotherAWSAccount where
  type
    Rs TransferDomainToAnotherAWSAccount =
      TransferDomainToAnotherAWSAccountResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          TransferDomainToAnotherAWSAccountResponse'
            Lude.<$> (x Lude..?> "Password")
            Lude.<*> (x Lude..?> "OperationId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders TransferDomainToAnotherAWSAccount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53Domains_v20140515.TransferDomainToAnotherAwsAccount" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TransferDomainToAnotherAWSAccount where
  toJSON TransferDomainToAnotherAWSAccount' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DomainName" Lude..= domainName),
            Lude.Just ("AccountId" Lude..= accountId)
          ]
      )

instance Lude.ToPath TransferDomainToAnotherAWSAccount where
  toPath = Lude.const "/"

instance Lude.ToQuery TransferDomainToAnotherAWSAccount where
  toQuery = Lude.const Lude.mempty

-- | The @TransferDomainToAnotherAwsAccount@ response includes the following elements.
--
-- /See:/ 'mkTransferDomainToAnotherAWSAccountResponse' smart constructor.
data TransferDomainToAnotherAWSAccountResponse = TransferDomainToAnotherAWSAccountResponse'
  { password ::
      Lude.Maybe
        Lude.Text,
    operationId ::
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
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransferDomainToAnotherAWSAccountResponse' with the minimum fields required to make a request.
--
-- * 'operationId' - Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
-- * 'password' - To finish transferring a domain to another AWS account, the account that the domain is being transferred to must submit an <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_AcceptDomainTransferFromAnotherAwsAccount.html AcceptDomainTransferFromAnotherAwsAccount> request. The request must include the value of the @Password@ element that was returned in the @TransferDomainToAnotherAwsAccount@ response.
-- * 'responseStatus' - The response status code.
mkTransferDomainToAnotherAWSAccountResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  TransferDomainToAnotherAWSAccountResponse
mkTransferDomainToAnotherAWSAccountResponse pResponseStatus_ =
  TransferDomainToAnotherAWSAccountResponse'
    { password =
        Lude.Nothing,
      operationId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | To finish transferring a domain to another AWS account, the account that the domain is being transferred to must submit an <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_AcceptDomainTransferFromAnotherAwsAccount.html AcceptDomainTransferFromAnotherAwsAccount> request. The request must include the value of the @Password@ element that was returned in the @TransferDomainToAnotherAwsAccount@ response.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdtaaarsPassword :: Lens.Lens' TransferDomainToAnotherAWSAccountResponse (Lude.Maybe Lude.Text)
tdtaaarsPassword = Lens.lens (password :: TransferDomainToAnotherAWSAccountResponse -> Lude.Maybe Lude.Text) (\s a -> s {password = a} :: TransferDomainToAnotherAWSAccountResponse)
{-# DEPRECATED tdtaaarsPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdtaaarsOperationId :: Lens.Lens' TransferDomainToAnotherAWSAccountResponse (Lude.Maybe Lude.Text)
tdtaaarsOperationId = Lens.lens (operationId :: TransferDomainToAnotherAWSAccountResponse -> Lude.Maybe Lude.Text) (\s a -> s {operationId = a} :: TransferDomainToAnotherAWSAccountResponse)
{-# DEPRECATED tdtaaarsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdtaaarsResponseStatus :: Lens.Lens' TransferDomainToAnotherAWSAccountResponse Lude.Int
tdtaaarsResponseStatus = Lens.lens (responseStatus :: TransferDomainToAnotherAWSAccountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: TransferDomainToAnotherAWSAccountResponse)
{-# DEPRECATED tdtaaarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
