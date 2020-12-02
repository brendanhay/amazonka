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
-- Module      : Network.AWS.Route53Domains.CancelDomainTransferToAnotherAWSAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the transfer of a domain from the current AWS account to another AWS account. You initiate a transfer between AWS accounts using <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> .
--
--
-- /Important:/ You must cancel the transfer before the other AWS account accepts the transfer using <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_AcceptDomainTransferFromAnotherAwsAccount.html AcceptDomainTransferFromAnotherAwsAccount> .
--
-- Use either <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ListOperations.html ListOperations> or <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> to determine whether the operation succeeded. <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> provides additional information, for example, @Domain Transfer from Aws Account 111122223333 has been cancelled@ .
module Network.AWS.Route53Domains.CancelDomainTransferToAnotherAWSAccount
  ( -- * Creating a Request
    cancelDomainTransferToAnotherAWSAccount,
    CancelDomainTransferToAnotherAWSAccount,

    -- * Request Lenses
    cdttaaaDomainName,

    -- * Destructuring the Response
    cancelDomainTransferToAnotherAWSAccountResponse,
    CancelDomainTransferToAnotherAWSAccountResponse,

    -- * Response Lenses
    cdttaaarsOperationId,
    cdttaaarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53Domains.Types

-- | The CancelDomainTransferToAnotherAwsAccount request includes the following element.
--
--
--
-- /See:/ 'cancelDomainTransferToAnotherAWSAccount' smart constructor.
newtype CancelDomainTransferToAnotherAWSAccount = CancelDomainTransferToAnotherAWSAccount'
  { _cdttaaaDomainName ::
      Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'CancelDomainTransferToAnotherAWSAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdttaaaDomainName' - The name of the domain for which you want to cancel the transfer to another AWS account.
cancelDomainTransferToAnotherAWSAccount ::
  -- | 'cdttaaaDomainName'
  Text ->
  CancelDomainTransferToAnotherAWSAccount
cancelDomainTransferToAnotherAWSAccount pDomainName_ =
  CancelDomainTransferToAnotherAWSAccount'
    { _cdttaaaDomainName =
        pDomainName_
    }

-- | The name of the domain for which you want to cancel the transfer to another AWS account.
cdttaaaDomainName :: Lens' CancelDomainTransferToAnotherAWSAccount Text
cdttaaaDomainName = lens _cdttaaaDomainName (\s a -> s {_cdttaaaDomainName = a})

instance AWSRequest CancelDomainTransferToAnotherAWSAccount where
  type
    Rs CancelDomainTransferToAnotherAWSAccount =
      CancelDomainTransferToAnotherAWSAccountResponse
  request = postJSON route53Domains
  response =
    receiveJSON
      ( \s h x ->
          CancelDomainTransferToAnotherAWSAccountResponse'
            <$> (x .?> "OperationId") <*> (pure (fromEnum s))
      )

instance Hashable CancelDomainTransferToAnotherAWSAccount

instance NFData CancelDomainTransferToAnotherAWSAccount

instance ToHeaders CancelDomainTransferToAnotherAWSAccount where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "Route53Domains_v20140515.CancelDomainTransferToAnotherAwsAccount" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CancelDomainTransferToAnotherAWSAccount where
  toJSON CancelDomainTransferToAnotherAWSAccount' {..} =
    object (catMaybes [Just ("DomainName" .= _cdttaaaDomainName)])

instance ToPath CancelDomainTransferToAnotherAWSAccount where
  toPath = const "/"

instance ToQuery CancelDomainTransferToAnotherAWSAccount where
  toQuery = const mempty

-- | The @CancelDomainTransferToAnotherAwsAccount@ response includes the following element.
--
--
--
-- /See:/ 'cancelDomainTransferToAnotherAWSAccountResponse' smart constructor.
data CancelDomainTransferToAnotherAWSAccountResponse = CancelDomainTransferToAnotherAWSAccountResponse'
  { _cdttaaarsOperationId ::
      !( Maybe
           Text
       ),
    _cdttaaarsResponseStatus ::
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

-- | Creates a value of 'CancelDomainTransferToAnotherAWSAccountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdttaaarsOperationId' - The identifier that @TransferDomainToAnotherAwsAccount@ returned to track the progress of the request. Because the transfer request was canceled, the value is no longer valid, and you can't use @GetOperationDetail@ to query the operation status.
--
-- * 'cdttaaarsResponseStatus' - -- | The response status code.
cancelDomainTransferToAnotherAWSAccountResponse ::
  -- | 'cdttaaarsResponseStatus'
  Int ->
  CancelDomainTransferToAnotherAWSAccountResponse
cancelDomainTransferToAnotherAWSAccountResponse pResponseStatus_ =
  CancelDomainTransferToAnotherAWSAccountResponse'
    { _cdttaaarsOperationId =
        Nothing,
      _cdttaaarsResponseStatus = pResponseStatus_
    }

-- | The identifier that @TransferDomainToAnotherAwsAccount@ returned to track the progress of the request. Because the transfer request was canceled, the value is no longer valid, and you can't use @GetOperationDetail@ to query the operation status.
cdttaaarsOperationId :: Lens' CancelDomainTransferToAnotherAWSAccountResponse (Maybe Text)
cdttaaarsOperationId = lens _cdttaaarsOperationId (\s a -> s {_cdttaaarsOperationId = a})

-- | -- | The response status code.
cdttaaarsResponseStatus :: Lens' CancelDomainTransferToAnotherAWSAccountResponse Int
cdttaaarsResponseStatus = lens _cdttaaarsResponseStatus (\s a -> s {_cdttaaarsResponseStatus = a})

instance NFData CancelDomainTransferToAnotherAWSAccountResponse
