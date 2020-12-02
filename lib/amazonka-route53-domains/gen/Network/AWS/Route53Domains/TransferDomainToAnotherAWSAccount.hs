{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
--     * You can cancel the transfer before the other account accepts it. See <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_CancelDomainTransferToAnotherAwsAccount.html CancelDomainTransferToAnotherAwsAccount> .
--
--     * The other account can reject the transfer. See <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_RejectDomainTransferFromAnotherAwsAccount.html RejectDomainTransferFromAnotherAwsAccount> .
--
--
--
-- /Important:/ When you transfer a domain from one AWS account to another, Route 53 doesn't transfer the hosted zone that is associated with the domain. DNS resolution isn't affected if the domain and the hosted zone are owned by separate accounts, so transferring the hosted zone is optional. For information about transferring the hosted zone to another AWS account, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/hosted-zones-migrating.html Migrating a Hosted Zone to a Different AWS Account> in the /Amazon Route 53 Developer Guide/ .
--
-- Use either <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ListOperations.html ListOperations> or <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> to determine whether the operation succeeded. <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> provides additional information, for example, @Domain Transfer from Aws Account 111122223333 has been cancelled@ .
module Network.AWS.Route53Domains.TransferDomainToAnotherAWSAccount
  ( -- * Creating a Request
    transferDomainToAnotherAWSAccount,
    TransferDomainToAnotherAWSAccount,

    -- * Request Lenses
    tdtaaaDomainName,
    tdtaaaAccountId,

    -- * Destructuring the Response
    transferDomainToAnotherAWSAccountResponse,
    TransferDomainToAnotherAWSAccountResponse,

    -- * Response Lenses
    tdtaaarsPassword,
    tdtaaarsOperationId,
    tdtaaarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53Domains.Types

-- | The TransferDomainToAnotherAwsAccount request includes the following elements.
--
--
--
-- /See:/ 'transferDomainToAnotherAWSAccount' smart constructor.
data TransferDomainToAnotherAWSAccount = TransferDomainToAnotherAWSAccount'
  { _tdtaaaDomainName ::
      !Text,
    _tdtaaaAccountId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransferDomainToAnotherAWSAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdtaaaDomainName' - The name of the domain that you want to transfer from the current AWS account to another account.
--
-- * 'tdtaaaAccountId' - The account ID of the AWS account that you want to transfer the domain to, for example, @111122223333@ .
transferDomainToAnotherAWSAccount ::
  -- | 'tdtaaaDomainName'
  Text ->
  -- | 'tdtaaaAccountId'
  Text ->
  TransferDomainToAnotherAWSAccount
transferDomainToAnotherAWSAccount pDomainName_ pAccountId_ =
  TransferDomainToAnotherAWSAccount'
    { _tdtaaaDomainName =
        pDomainName_,
      _tdtaaaAccountId = pAccountId_
    }

-- | The name of the domain that you want to transfer from the current AWS account to another account.
tdtaaaDomainName :: Lens' TransferDomainToAnotherAWSAccount Text
tdtaaaDomainName = lens _tdtaaaDomainName (\s a -> s {_tdtaaaDomainName = a})

-- | The account ID of the AWS account that you want to transfer the domain to, for example, @111122223333@ .
tdtaaaAccountId :: Lens' TransferDomainToAnotherAWSAccount Text
tdtaaaAccountId = lens _tdtaaaAccountId (\s a -> s {_tdtaaaAccountId = a})

instance AWSRequest TransferDomainToAnotherAWSAccount where
  type
    Rs TransferDomainToAnotherAWSAccount =
      TransferDomainToAnotherAWSAccountResponse
  request = postJSON route53Domains
  response =
    receiveJSON
      ( \s h x ->
          TransferDomainToAnotherAWSAccountResponse'
            <$> (x .?> "Password")
            <*> (x .?> "OperationId")
            <*> (pure (fromEnum s))
      )

instance Hashable TransferDomainToAnotherAWSAccount

instance NFData TransferDomainToAnotherAWSAccount

instance ToHeaders TransferDomainToAnotherAWSAccount where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "Route53Domains_v20140515.TransferDomainToAnotherAwsAccount" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON TransferDomainToAnotherAWSAccount where
  toJSON TransferDomainToAnotherAWSAccount' {..} =
    object
      ( catMaybes
          [ Just ("DomainName" .= _tdtaaaDomainName),
            Just ("AccountId" .= _tdtaaaAccountId)
          ]
      )

instance ToPath TransferDomainToAnotherAWSAccount where
  toPath = const "/"

instance ToQuery TransferDomainToAnotherAWSAccount where
  toQuery = const mempty

-- | The @TransferDomainToAnotherAwsAccount@ response includes the following elements.
--
--
--
-- /See:/ 'transferDomainToAnotherAWSAccountResponse' smart constructor.
data TransferDomainToAnotherAWSAccountResponse = TransferDomainToAnotherAWSAccountResponse'
  { _tdtaaarsPassword ::
      !( Maybe
           Text
       ),
    _tdtaaarsOperationId ::
      !( Maybe
           Text
       ),
    _tdtaaarsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'TransferDomainToAnotherAWSAccountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdtaaarsPassword' - To finish transferring a domain to another AWS account, the account that the domain is being transferred to must submit an <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_AcceptDomainTransferFromAnotherAwsAccount.html AcceptDomainTransferFromAnotherAwsAccount> request. The request must include the value of the @Password@ element that was returned in the @TransferDomainToAnotherAwsAccount@ response.
--
-- * 'tdtaaarsOperationId' - Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
--
-- * 'tdtaaarsResponseStatus' - -- | The response status code.
transferDomainToAnotherAWSAccountResponse ::
  -- | 'tdtaaarsResponseStatus'
  Int ->
  TransferDomainToAnotherAWSAccountResponse
transferDomainToAnotherAWSAccountResponse pResponseStatus_ =
  TransferDomainToAnotherAWSAccountResponse'
    { _tdtaaarsPassword =
        Nothing,
      _tdtaaarsOperationId = Nothing,
      _tdtaaarsResponseStatus = pResponseStatus_
    }

-- | To finish transferring a domain to another AWS account, the account that the domain is being transferred to must submit an <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_AcceptDomainTransferFromAnotherAwsAccount.html AcceptDomainTransferFromAnotherAwsAccount> request. The request must include the value of the @Password@ element that was returned in the @TransferDomainToAnotherAwsAccount@ response.
tdtaaarsPassword :: Lens' TransferDomainToAnotherAWSAccountResponse (Maybe Text)
tdtaaarsPassword = lens _tdtaaarsPassword (\s a -> s {_tdtaaarsPassword = a})

-- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
tdtaaarsOperationId :: Lens' TransferDomainToAnotherAWSAccountResponse (Maybe Text)
tdtaaarsOperationId = lens _tdtaaarsOperationId (\s a -> s {_tdtaaarsOperationId = a})

-- | -- | The response status code.
tdtaaarsResponseStatus :: Lens' TransferDomainToAnotherAWSAccountResponse Int
tdtaaarsResponseStatus = lens _tdtaaarsResponseStatus (\s a -> s {_tdtaaarsResponseStatus = a})

instance NFData TransferDomainToAnotherAWSAccountResponse
