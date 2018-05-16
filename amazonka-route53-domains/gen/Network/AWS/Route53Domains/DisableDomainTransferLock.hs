{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.DisableDomainTransferLock
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation removes the transfer lock on the domain (specifically the @clientTransferProhibited@ status) to allow domain transfers. We recommend you refrain from performing this action unless you intend to transfer the domain to a different registrar. Successful submission returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.
--
--
module Network.AWS.Route53Domains.DisableDomainTransferLock
    (
    -- * Creating a Request
      disableDomainTransferLock
    , DisableDomainTransferLock
    -- * Request Lenses
    , ddtlDomainName

    -- * Destructuring the Response
    , disableDomainTransferLockResponse
    , DisableDomainTransferLockResponse
    -- * Response Lenses
    , ddtlrsResponseStatus
    , ddtlrsOperationId
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53Domains.Types
import Network.AWS.Route53Domains.Types.Product

-- | The DisableDomainTransferLock request includes the following element.
--
--
--
-- /See:/ 'disableDomainTransferLock' smart constructor.
newtype DisableDomainTransferLock = DisableDomainTransferLock'
  { _ddtlDomainName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableDomainTransferLock' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddtlDomainName' - The name of the domain that you want to remove the transfer lock for.
disableDomainTransferLock
    :: Text -- ^ 'ddtlDomainName'
    -> DisableDomainTransferLock
disableDomainTransferLock pDomainName_ =
  DisableDomainTransferLock' {_ddtlDomainName = pDomainName_}


-- | The name of the domain that you want to remove the transfer lock for.
ddtlDomainName :: Lens' DisableDomainTransferLock Text
ddtlDomainName = lens _ddtlDomainName (\ s a -> s{_ddtlDomainName = a})

instance AWSRequest DisableDomainTransferLock where
        type Rs DisableDomainTransferLock =
             DisableDomainTransferLockResponse
        request = postJSON route53Domains
        response
          = receiveJSON
              (\ s h x ->
                 DisableDomainTransferLockResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "OperationId"))

instance Hashable DisableDomainTransferLock where

instance NFData DisableDomainTransferLock where

instance ToHeaders DisableDomainTransferLock where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.DisableDomainTransferLock"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisableDomainTransferLock where
        toJSON DisableDomainTransferLock'{..}
          = object
              (catMaybes [Just ("DomainName" .= _ddtlDomainName)])

instance ToPath DisableDomainTransferLock where
        toPath = const "/"

instance ToQuery DisableDomainTransferLock where
        toQuery = const mempty

-- | The DisableDomainTransferLock response includes the following element.
--
--
--
-- /See:/ 'disableDomainTransferLockResponse' smart constructor.
data DisableDomainTransferLockResponse = DisableDomainTransferLockResponse'
  { _ddtlrsResponseStatus :: !Int
  , _ddtlrsOperationId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableDomainTransferLockResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddtlrsResponseStatus' - -- | The response status code.
--
-- * 'ddtlrsOperationId' - Identifier for tracking the progress of the request. To use this ID to query the operation status, use 'GetOperationDetail' .
disableDomainTransferLockResponse
    :: Int -- ^ 'ddtlrsResponseStatus'
    -> Text -- ^ 'ddtlrsOperationId'
    -> DisableDomainTransferLockResponse
disableDomainTransferLockResponse pResponseStatus_ pOperationId_ =
  DisableDomainTransferLockResponse'
    { _ddtlrsResponseStatus = pResponseStatus_
    , _ddtlrsOperationId = pOperationId_
    }


-- | -- | The response status code.
ddtlrsResponseStatus :: Lens' DisableDomainTransferLockResponse Int
ddtlrsResponseStatus = lens _ddtlrsResponseStatus (\ s a -> s{_ddtlrsResponseStatus = a})

-- | Identifier for tracking the progress of the request. To use this ID to query the operation status, use 'GetOperationDetail' .
ddtlrsOperationId :: Lens' DisableDomainTransferLockResponse Text
ddtlrsOperationId = lens _ddtlrsOperationId (\ s a -> s{_ddtlrsOperationId = a})

instance NFData DisableDomainTransferLockResponse
         where
