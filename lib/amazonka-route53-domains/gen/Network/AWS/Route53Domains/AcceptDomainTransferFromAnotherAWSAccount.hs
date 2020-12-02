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
-- Module      : Network.AWS.Route53Domains.AcceptDomainTransferFromAnotherAWSAccount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts the transfer of a domain from another AWS account to the current AWS account. You initiate a transfer between AWS accounts using <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> .
--
--
-- Use either <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_ListOperations.html ListOperations> or <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> to determine whether the operation succeeded. <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> provides additional information, for example, @Domain Transfer from Aws Account 111122223333 has been cancelled@ .
module Network.AWS.Route53Domains.AcceptDomainTransferFromAnotherAWSAccount
  ( -- * Creating a Request
    acceptDomainTransferFromAnotherAWSAccount,
    AcceptDomainTransferFromAnotherAWSAccount,

    -- * Request Lenses
    adtfaaaDomainName,
    adtfaaaPassword,

    -- * Destructuring the Response
    acceptDomainTransferFromAnotherAWSAccountResponse,
    AcceptDomainTransferFromAnotherAWSAccountResponse,

    -- * Response Lenses
    adtfaaarsOperationId,
    adtfaaarsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53Domains.Types

-- | The AcceptDomainTransferFromAnotherAwsAccount request includes the following elements.
--
--
--
-- /See:/ 'acceptDomainTransferFromAnotherAWSAccount' smart constructor.
data AcceptDomainTransferFromAnotherAWSAccount = AcceptDomainTransferFromAnotherAWSAccount'
  { _adtfaaaDomainName ::
      !Text,
    _adtfaaaPassword ::
      !Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'AcceptDomainTransferFromAnotherAWSAccount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adtfaaaDomainName' - The name of the domain that was specified when another AWS account submitted a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> request.
--
-- * 'adtfaaaPassword' - The password that was returned by the <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> request.
acceptDomainTransferFromAnotherAWSAccount ::
  -- | 'adtfaaaDomainName'
  Text ->
  -- | 'adtfaaaPassword'
  Text ->
  AcceptDomainTransferFromAnotherAWSAccount
acceptDomainTransferFromAnotherAWSAccount pDomainName_ pPassword_ =
  AcceptDomainTransferFromAnotherAWSAccount'
    { _adtfaaaDomainName =
        pDomainName_,
      _adtfaaaPassword = pPassword_
    }

-- | The name of the domain that was specified when another AWS account submitted a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> request.
adtfaaaDomainName :: Lens' AcceptDomainTransferFromAnotherAWSAccount Text
adtfaaaDomainName = lens _adtfaaaDomainName (\s a -> s {_adtfaaaDomainName = a})

-- | The password that was returned by the <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_TransferDomainToAnotherAwsAccount.html TransferDomainToAnotherAwsAccount> request.
adtfaaaPassword :: Lens' AcceptDomainTransferFromAnotherAWSAccount Text
adtfaaaPassword = lens _adtfaaaPassword (\s a -> s {_adtfaaaPassword = a})

instance AWSRequest AcceptDomainTransferFromAnotherAWSAccount where
  type
    Rs AcceptDomainTransferFromAnotherAWSAccount =
      AcceptDomainTransferFromAnotherAWSAccountResponse
  request = postJSON route53Domains
  response =
    receiveJSON
      ( \s h x ->
          AcceptDomainTransferFromAnotherAWSAccountResponse'
            <$> (x .?> "OperationId") <*> (pure (fromEnum s))
      )

instance Hashable AcceptDomainTransferFromAnotherAWSAccount

instance NFData AcceptDomainTransferFromAnotherAWSAccount

instance ToHeaders AcceptDomainTransferFromAnotherAWSAccount where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "Route53Domains_v20140515.AcceptDomainTransferFromAnotherAwsAccount" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON AcceptDomainTransferFromAnotherAWSAccount where
  toJSON AcceptDomainTransferFromAnotherAWSAccount' {..} =
    object
      ( catMaybes
          [ Just ("DomainName" .= _adtfaaaDomainName),
            Just ("Password" .= _adtfaaaPassword)
          ]
      )

instance ToPath AcceptDomainTransferFromAnotherAWSAccount where
  toPath = const "/"

instance ToQuery AcceptDomainTransferFromAnotherAWSAccount where
  toQuery = const mempty

-- | The AcceptDomainTransferFromAnotherAwsAccount response includes the following element.
--
--
--
-- /See:/ 'acceptDomainTransferFromAnotherAWSAccountResponse' smart constructor.
data AcceptDomainTransferFromAnotherAWSAccountResponse = AcceptDomainTransferFromAnotherAWSAccountResponse'
  { _adtfaaarsOperationId ::
      !( Maybe
           Text
       ),
    _adtfaaarsResponseStatus ::
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

-- | Creates a value of 'AcceptDomainTransferFromAnotherAWSAccountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'adtfaaarsOperationId' - Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
--
-- * 'adtfaaarsResponseStatus' - -- | The response status code.
acceptDomainTransferFromAnotherAWSAccountResponse ::
  -- | 'adtfaaarsResponseStatus'
  Int ->
  AcceptDomainTransferFromAnotherAWSAccountResponse
acceptDomainTransferFromAnotherAWSAccountResponse pResponseStatus_ =
  AcceptDomainTransferFromAnotherAWSAccountResponse'
    { _adtfaaarsOperationId =
        Nothing,
      _adtfaaarsResponseStatus = pResponseStatus_
    }

-- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
adtfaaarsOperationId :: Lens' AcceptDomainTransferFromAnotherAWSAccountResponse (Maybe Text)
adtfaaarsOperationId = lens _adtfaaarsOperationId (\s a -> s {_adtfaaarsOperationId = a})

-- | -- | The response status code.
adtfaaarsResponseStatus :: Lens' AcceptDomainTransferFromAnotherAWSAccountResponse Int
adtfaaarsResponseStatus = lens _adtfaaarsResponseStatus (\s a -> s {_adtfaaarsResponseStatus = a})

instance NFData AcceptDomainTransferFromAnotherAWSAccountResponse
