{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.DisableDomainTransferLock
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation removes the transfer lock on the domain (specifically the
-- @clientTransferProhibited@ status) to allow domain transfers. We
-- recommend you refrain from performing this action unless you intend to
-- transfer the domain to a different registrar. Successful submission
-- returns an operation ID that you can use to track the progress and
-- completion of the action. If the request is not completed successfully,
-- the domain registrant will be notified by email.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/api-DisableDomainTransferLock.html AWS API Reference> for DisableDomainTransferLock.
module Network.AWS.Route53Domains.DisableDomainTransferLock
    (
    -- * Creating a Request
      DisableDomainTransferLock
    , disableDomainTransferLock
    -- * Request Lenses
    , ddtlDomainName

    -- * Destructuring the Response
    , DisableDomainTransferLockResponse
    , disableDomainTransferLockResponse
    -- * Response Lenses
    , ddtlrsStatus
    , ddtlrsOperationId
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types
import           Network.AWS.Route53Domains.Types.Product

-- | The DisableDomainTransferLock request includes the following element.
--
-- /See:/ 'disableDomainTransferLock' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddtlDomainName'
newtype DisableDomainTransferLock = DisableDomainTransferLock'
    { _ddtlDomainName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisableDomainTransferLock' smart constructor.
disableDomainTransferLock :: Text -> DisableDomainTransferLock
disableDomainTransferLock pDomainName_ =
    DisableDomainTransferLock'
    { _ddtlDomainName = pDomainName_
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
ddtlDomainName :: Lens' DisableDomainTransferLock Text
ddtlDomainName = lens _ddtlDomainName (\ s a -> s{_ddtlDomainName = a});

instance AWSRequest DisableDomainTransferLock where
        type Sv DisableDomainTransferLock = Route53Domains
        type Rs DisableDomainTransferLock =
             DisableDomainTransferLockResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DisableDomainTransferLockResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "OperationId"))

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
          = object ["DomainName" .= _ddtlDomainName]

instance ToPath DisableDomainTransferLock where
        toPath = const "/"

instance ToQuery DisableDomainTransferLock where
        toQuery = const mempty

-- | The DisableDomainTransferLock response includes the following element.
--
-- /See:/ 'disableDomainTransferLockResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddtlrsStatus'
--
-- * 'ddtlrsOperationId'
data DisableDomainTransferLockResponse = DisableDomainTransferLockResponse'
    { _ddtlrsStatus      :: !Int
    , _ddtlrsOperationId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisableDomainTransferLockResponse' smart constructor.
disableDomainTransferLockResponse :: Int -> Text -> DisableDomainTransferLockResponse
disableDomainTransferLockResponse pStatus_ pOperationId_ =
    DisableDomainTransferLockResponse'
    { _ddtlrsStatus = pStatus_
    , _ddtlrsOperationId = pOperationId_
    }

-- | Undocumented member.
ddtlrsStatus :: Lens' DisableDomainTransferLockResponse Int
ddtlrsStatus = lens _ddtlrsStatus (\ s a -> s{_ddtlrsStatus = a});

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
ddtlrsOperationId :: Lens' DisableDomainTransferLockResponse Text
ddtlrsOperationId = lens _ddtlrsOperationId (\ s a -> s{_ddtlrsOperationId = a});
