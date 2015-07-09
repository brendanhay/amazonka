{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.EnableDomainTransferLock
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | This operation sets the transfer lock on the domain (specifically the
-- @clientTransferProhibited@ status) to prevent domain transfers.
-- Successful submission returns an operation ID that you can use to track
-- the progress and completion of the action. If the request is not
-- completed successfully, the domain registrant will be notified by email.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-EnableDomainTransferLock.html>
module Network.AWS.Route53Domains.EnableDomainTransferLock
    (
    -- * Request
      EnableDomainTransferLock
    -- ** Request constructor
    , enableDomainTransferLock
    -- ** Request lenses
    , edtlDomainName

    -- * Response
    , EnableDomainTransferLockResponse
    -- ** Response constructor
    , enableDomainTransferLockResponse
    -- ** Response lenses
    , edtlrStatus
    , edtlrOperationId
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types

-- | The EnableDomainTransferLock request includes the following element.
--
-- /See:/ 'enableDomainTransferLock' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'edtlDomainName'
newtype EnableDomainTransferLock = EnableDomainTransferLock'
    { _edtlDomainName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableDomainTransferLock' smart constructor.
enableDomainTransferLock :: Text -> EnableDomainTransferLock
enableDomainTransferLock pDomainName =
    EnableDomainTransferLock'
    { _edtlDomainName = pDomainName
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
        type Sv EnableDomainTransferLock = Route53Domains
        type Rs EnableDomainTransferLock =
             EnableDomainTransferLockResponse
        request = postJSON
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
          = object ["DomainName" .= _edtlDomainName]

instance ToPath EnableDomainTransferLock where
        toPath = const "/"

instance ToQuery EnableDomainTransferLock where
        toQuery = const mempty

-- | The EnableDomainTransferLock response includes the following elements.
--
-- /See:/ 'enableDomainTransferLockResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'edtlrStatus'
--
-- * 'edtlrOperationId'
data EnableDomainTransferLockResponse = EnableDomainTransferLockResponse'
    { _edtlrStatus      :: !Int
    , _edtlrOperationId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableDomainTransferLockResponse' smart constructor.
enableDomainTransferLockResponse :: Int -> Text -> EnableDomainTransferLockResponse
enableDomainTransferLockResponse pStatus pOperationId =
    EnableDomainTransferLockResponse'
    { _edtlrStatus = pStatus
    , _edtlrOperationId = pOperationId
    }

-- | FIXME: Undocumented member.
edtlrStatus :: Lens' EnableDomainTransferLockResponse Int
edtlrStatus = lens _edtlrStatus (\ s a -> s{_edtlrStatus = a});

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
edtlrOperationId :: Lens' EnableDomainTransferLockResponse Text
edtlrOperationId = lens _edtlrOperationId (\ s a -> s{_edtlrOperationId = a});
