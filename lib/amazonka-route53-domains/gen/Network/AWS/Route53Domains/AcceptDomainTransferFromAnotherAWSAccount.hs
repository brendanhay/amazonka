{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.AcceptDomainTransferFromAnotherAWSAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts the transfer of a domain from another AWS account to the current AWS account. You initiate a transfer between AWS accounts using <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> .
--
-- Use either <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ListOperations.html ListOperations> or <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> to determine whether the operation succeeded. <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> provides additional information, for example, @Domain Transfer from Aws Account 111122223333 has been cancelled@ .
module Network.AWS.Route53Domains.AcceptDomainTransferFromAnotherAWSAccount
  ( -- * Creating a request
    AcceptDomainTransferFromAnotherAWSAccount (..),
    mkAcceptDomainTransferFromAnotherAWSAccount,

    -- ** Request lenses
    adtfaaaDomainName,
    adtfaaaPassword,

    -- * Destructuring the response
    AcceptDomainTransferFromAnotherAWSAccountResponse (..),
    mkAcceptDomainTransferFromAnotherAWSAccountResponse,

    -- ** Response lenses
    adtfaaarsOperationId,
    adtfaaarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | The AcceptDomainTransferFromAnotherAwsAccount request includes the following elements.
--
-- /See:/ 'mkAcceptDomainTransferFromAnotherAWSAccount' smart constructor.
data AcceptDomainTransferFromAnotherAWSAccount = AcceptDomainTransferFromAnotherAWSAccount'
  { -- | The name of the domain that was specified when another AWS account submitted a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> request.
    domainName :: Lude.Text,
    -- | The password that was returned by the <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> request.
    password :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcceptDomainTransferFromAnotherAWSAccount' with the minimum fields required to make a request.
--
-- * 'domainName' - The name of the domain that was specified when another AWS account submitted a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> request.
-- * 'password' - The password that was returned by the <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> request.
mkAcceptDomainTransferFromAnotherAWSAccount ::
  -- | 'domainName'
  Lude.Text ->
  -- | 'password'
  Lude.Text ->
  AcceptDomainTransferFromAnotherAWSAccount
mkAcceptDomainTransferFromAnotherAWSAccount pDomainName_ pPassword_ =
  AcceptDomainTransferFromAnotherAWSAccount'
    { domainName =
        pDomainName_,
      password = pPassword_
    }

-- | The name of the domain that was specified when another AWS account submitted a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> request.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adtfaaaDomainName :: Lens.Lens' AcceptDomainTransferFromAnotherAWSAccount Lude.Text
adtfaaaDomainName = Lens.lens (domainName :: AcceptDomainTransferFromAnotherAWSAccount -> Lude.Text) (\s a -> s {domainName = a} :: AcceptDomainTransferFromAnotherAWSAccount)
{-# DEPRECATED adtfaaaDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The password that was returned by the <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> request.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adtfaaaPassword :: Lens.Lens' AcceptDomainTransferFromAnotherAWSAccount Lude.Text
adtfaaaPassword = Lens.lens (password :: AcceptDomainTransferFromAnotherAWSAccount -> Lude.Text) (\s a -> s {password = a} :: AcceptDomainTransferFromAnotherAWSAccount)
{-# DEPRECATED adtfaaaPassword "Use generic-lens or generic-optics with 'password' instead." #-}

instance Lude.AWSRequest AcceptDomainTransferFromAnotherAWSAccount where
  type
    Rs AcceptDomainTransferFromAnotherAWSAccount =
      AcceptDomainTransferFromAnotherAWSAccountResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          AcceptDomainTransferFromAnotherAWSAccountResponse'
            Lude.<$> (x Lude..?> "OperationId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AcceptDomainTransferFromAnotherAWSAccount where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53Domains_v20140515.AcceptDomainTransferFromAnotherAwsAccount" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AcceptDomainTransferFromAnotherAWSAccount where
  toJSON AcceptDomainTransferFromAnotherAWSAccount' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DomainName" Lude..= domainName),
            Lude.Just ("Password" Lude..= password)
          ]
      )

instance Lude.ToPath AcceptDomainTransferFromAnotherAWSAccount where
  toPath = Lude.const "/"

instance Lude.ToQuery AcceptDomainTransferFromAnotherAWSAccount where
  toQuery = Lude.const Lude.mempty

-- | The AcceptDomainTransferFromAnotherAwsAccount response includes the following element.
--
-- /See:/ 'mkAcceptDomainTransferFromAnotherAWSAccountResponse' smart constructor.
data AcceptDomainTransferFromAnotherAWSAccountResponse = AcceptDomainTransferFromAnotherAWSAccountResponse'
  { -- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
    operationId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AcceptDomainTransferFromAnotherAWSAccountResponse' with the minimum fields required to make a request.
--
-- * 'operationId' - Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
-- * 'responseStatus' - The response status code.
mkAcceptDomainTransferFromAnotherAWSAccountResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AcceptDomainTransferFromAnotherAWSAccountResponse
mkAcceptDomainTransferFromAnotherAWSAccountResponse
  pResponseStatus_ =
    AcceptDomainTransferFromAnotherAWSAccountResponse'
      { operationId =
          Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adtfaaarsOperationId :: Lens.Lens' AcceptDomainTransferFromAnotherAWSAccountResponse (Lude.Maybe Lude.Text)
adtfaaarsOperationId = Lens.lens (operationId :: AcceptDomainTransferFromAnotherAWSAccountResponse -> Lude.Maybe Lude.Text) (\s a -> s {operationId = a} :: AcceptDomainTransferFromAnotherAWSAccountResponse)
{-# DEPRECATED adtfaaarsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adtfaaarsResponseStatus :: Lens.Lens' AcceptDomainTransferFromAnotherAWSAccountResponse Lude.Int
adtfaaarsResponseStatus = Lens.lens (responseStatus :: AcceptDomainTransferFromAnotherAWSAccountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AcceptDomainTransferFromAnotherAWSAccountResponse)
{-# DEPRECATED adtfaaarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
