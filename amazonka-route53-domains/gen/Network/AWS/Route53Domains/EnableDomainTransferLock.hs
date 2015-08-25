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
-- Module      : Network.AWS.Route53Domains.EnableDomainTransferLock
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation sets the transfer lock on the domain (specifically the
-- 'clientTransferProhibited' status) to prevent domain transfers.
-- Successful submission returns an operation ID that you can use to track
-- the progress and completion of the action. If the request is not
-- completed successfully, the domain registrant will be notified by email.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/api-EnableDomainTransferLock.html AWS API Reference> for EnableDomainTransferLock.
module Network.AWS.Route53Domains.EnableDomainTransferLock
    (
    -- * Creating a Request
      enableDomainTransferLock
    , EnableDomainTransferLock
    -- * Request Lenses
    , edtlDomainName

    -- * Destructuring the Response
    , enableDomainTransferLockResponse
    , EnableDomainTransferLockResponse
    -- * Response Lenses
    , edtlrsStatus
    , edtlrsOperationId
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types
import           Network.AWS.Route53Domains.Types.Product

-- | The EnableDomainTransferLock request includes the following element.
--
-- /See:/ 'enableDomainTransferLock' smart constructor.
newtype EnableDomainTransferLock = EnableDomainTransferLock'
    { _edtlDomainName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnableDomainTransferLock' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edtlDomainName'
enableDomainTransferLock
    :: Text -- ^ 'edtlDomainName'
    -> EnableDomainTransferLock
enableDomainTransferLock pDomainName_ =
    EnableDomainTransferLock'
    { _edtlDomainName = pDomainName_
    }

-- | The name of a domain.
--
-- Type: String
--
-- Default: None
--
-- Constraints: The domain name can contain only the letters a through z,
-- the numbers 0 through 9, and hyphen (-). Internationalized Domain Names
-- are not supported.
--
-- Required: Yes
edtlDomainName :: Lens' EnableDomainTransferLock Text
edtlDomainName = lens _edtlDomainName (\ s a -> s{_edtlDomainName = a});

instance AWSRequest EnableDomainTransferLock where
        type Rs EnableDomainTransferLock =
             EnableDomainTransferLockResponse
        request = postJSON route53Domains
        response
          = receiveJSON
              (\ s h x ->
                 EnableDomainTransferLockResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "OperationId"))

instance ToHeaders EnableDomainTransferLock where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.EnableDomainTransferLock"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON EnableDomainTransferLock where
        toJSON EnableDomainTransferLock'{..}
          = object
              (catMaybes [Just ("DomainName" .= _edtlDomainName)])

instance ToPath EnableDomainTransferLock where
        toPath = const "/"

instance ToQuery EnableDomainTransferLock where
        toQuery = const mempty

-- | The EnableDomainTransferLock response includes the following elements.
--
-- /See:/ 'enableDomainTransferLockResponse' smart constructor.
data EnableDomainTransferLockResponse = EnableDomainTransferLockResponse'
    { _edtlrsStatus      :: !Int
    , _edtlrsOperationId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnableDomainTransferLockResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edtlrsStatus'
--
-- * 'edtlrsOperationId'
enableDomainTransferLockResponse
    :: Int -- ^ 'edtlrsStatus'
    -> Text -- ^ 'edtlrsOperationId'
    -> EnableDomainTransferLockResponse
enableDomainTransferLockResponse pStatus_ pOperationId_ =
    EnableDomainTransferLockResponse'
    { _edtlrsStatus = pStatus_
    , _edtlrsOperationId = pOperationId_
    }

-- | The response status code.
edtlrsStatus :: Lens' EnableDomainTransferLockResponse Int
edtlrsStatus = lens _edtlrsStatus (\ s a -> s{_edtlrsStatus = a});

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
edtlrsOperationId :: Lens' EnableDomainTransferLockResponse Text
edtlrsOperationId = lens _edtlrsOperationId (\ s a -> s{_edtlrsOperationId = a});
