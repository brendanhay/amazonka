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
-- Module      : Network.AWS.Route53Domains.RejectDomainTransferFromAnotherAWSAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects the transfer of a domain from another AWS account to the current AWS account. You initiate a transfer between AWS accounts using <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> .
--
--
-- Use either <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ListOperations.html ListOperations> or <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> to determine whether the operation succeeded. <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> provides additional information, for example, @Domain Transfer from Aws Account 111122223333 has been cancelled@ .
module Network.AWS.Route53Domains.RejectDomainTransferFromAnotherAWSAccount
  ( -- * Creating a Request
    rejectDomainTransferFromAnotherAWSAccount,
    RejectDomainTransferFromAnotherAWSAccount,

    -- * Request Lenses
    rdtfaaaDomainName,

    -- * Destructuring the Response
    rejectDomainTransferFromAnotherAWSAccountResponse,
    RejectDomainTransferFromAnotherAWSAccountResponse,

    -- * Response Lenses
    rdtfaaarsOperationId,
    rdtfaaarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53Domains.Types

-- | The RejectDomainTransferFromAnotherAwsAccount request includes the following element.
--
--
--
-- /See:/ 'rejectDomainTransferFromAnotherAWSAccount' smart constructor.
newtype RejectDomainTransferFromAnotherAWSAccount = RejectDomainTransferFromAnotherAWSAccount'
  { _rdtfaaaDomainName ::
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

-- | Creates a value of 'RejectDomainTransferFromAnotherAWSAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdtfaaaDomainName' - The name of the domain that was specified when another AWS account submitted a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> request.
rejectDomainTransferFromAnotherAWSAccount ::
  -- | 'rdtfaaaDomainName'
  Text ->
  RejectDomainTransferFromAnotherAWSAccount
rejectDomainTransferFromAnotherAWSAccount pDomainName_ =
  RejectDomainTransferFromAnotherAWSAccount'
    { _rdtfaaaDomainName =
        pDomainName_
    }

-- | The name of the domain that was specified when another AWS account submitted a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> request.
rdtfaaaDomainName :: Lens' RejectDomainTransferFromAnotherAWSAccount Text
rdtfaaaDomainName = lens _rdtfaaaDomainName (\s a -> s {_rdtfaaaDomainName = a})

instance AWSRequest RejectDomainTransferFromAnotherAWSAccount where
  type
    Rs RejectDomainTransferFromAnotherAWSAccount =
      RejectDomainTransferFromAnotherAWSAccountResponse
  request = postJSON route53Domains
  response =
    receiveJSON
      ( \s h x ->
          RejectDomainTransferFromAnotherAWSAccountResponse'
            <$> (x .?> "OperationId") <*> (pure (fromEnum s))
      )

instance Hashable RejectDomainTransferFromAnotherAWSAccount

instance NFData RejectDomainTransferFromAnotherAWSAccount

instance ToHeaders RejectDomainTransferFromAnotherAWSAccount where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "Route53Domains_v20140515.RejectDomainTransferFromAnotherAwsAccount" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON RejectDomainTransferFromAnotherAWSAccount where
  toJSON RejectDomainTransferFromAnotherAWSAccount' {..} =
    object (catMaybes [Just ("DomainName" .= _rdtfaaaDomainName)])

instance ToPath RejectDomainTransferFromAnotherAWSAccount where
  toPath = const "/"

instance ToQuery RejectDomainTransferFromAnotherAWSAccount where
  toQuery = const mempty

-- | The RejectDomainTransferFromAnotherAwsAccount response includes the following element.
--
--
--
-- /See:/ 'rejectDomainTransferFromAnotherAWSAccountResponse' smart constructor.
data RejectDomainTransferFromAnotherAWSAccountResponse = RejectDomainTransferFromAnotherAWSAccountResponse'
  { _rdtfaaarsOperationId ::
      !( Maybe
           Text
       ),
    _rdtfaaarsResponseStatus ::
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

-- | Creates a value of 'RejectDomainTransferFromAnotherAWSAccountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdtfaaarsOperationId' - The identifier that @TransferDomainToAnotherAwsAccount@ returned to track the progress of the request. Because the transfer request was rejected, the value is no longer valid, and you can't use @GetOperationDetail@ to query the operation status.
--
-- * 'rdtfaaarsResponseStatus' - -- | The response status code.
rejectDomainTransferFromAnotherAWSAccountResponse ::
  -- | 'rdtfaaarsResponseStatus'
  Int ->
  RejectDomainTransferFromAnotherAWSAccountResponse
rejectDomainTransferFromAnotherAWSAccountResponse pResponseStatus_ =
  RejectDomainTransferFromAnotherAWSAccountResponse'
    { _rdtfaaarsOperationId =
        Nothing,
      _rdtfaaarsResponseStatus = pResponseStatus_
    }

-- | The identifier that @TransferDomainToAnotherAwsAccount@ returned to track the progress of the request. Because the transfer request was rejected, the value is no longer valid, and you can't use @GetOperationDetail@ to query the operation status.
rdtfaaarsOperationId :: Lens' RejectDomainTransferFromAnotherAWSAccountResponse (Maybe Text)
rdtfaaarsOperationId = lens _rdtfaaarsOperationId (\s a -> s {_rdtfaaarsOperationId = a})

-- | -- | The response status code.
rdtfaaarsResponseStatus :: Lens' RejectDomainTransferFromAnotherAWSAccountResponse Int
rdtfaaarsResponseStatus = lens _rdtfaaarsResponseStatus (\s a -> s {_rdtfaaarsResponseStatus = a})

instance NFData RejectDomainTransferFromAnotherAWSAccountResponse
