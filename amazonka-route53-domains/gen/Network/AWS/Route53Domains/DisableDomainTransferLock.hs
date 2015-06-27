{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Route53Domains.DisableDomainTransferLock
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation removes the transfer lock on the domain (specifically the
-- @clientTransferProhibited@ status) to allow domain transfers. We
-- recommend you refrain from performing this action unless you intend to
-- transfer the domain to a different registrar. Successful submission
-- returns an operation ID that you can use to track the progress and
-- completion of the action. If the request is not completed successfully,
-- the domain registrant will be notified by email.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-DisableDomainTransferLock.html>
module Network.AWS.Route53Domains.DisableDomainTransferLock
    (
    -- * Request
      DisableDomainTransferLock
    -- ** Request constructor
    , disableDomainTransferLock
    -- ** Request lenses
    , ddtlDomainName

    -- * Response
    , DisableDomainTransferLockResponse
    -- ** Response constructor
    , disableDomainTransferLockResponse
    -- ** Response lenses
    , ddtlrOperationId
    , ddtlrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types

-- | The DisableDomainTransferLock request includes the following element.
--
-- /See:/ 'disableDomainTransferLock' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddtlDomainName'
newtype DisableDomainTransferLock = DisableDomainTransferLock'
    { _ddtlDomainName :: Text
    } deriving (Eq,Read,Show)

-- | 'DisableDomainTransferLock' smart constructor.
disableDomainTransferLock :: Text -> DisableDomainTransferLock
disableDomainTransferLock pDomainName =
    DisableDomainTransferLock'
    { _ddtlDomainName = pDomainName
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
                   (x .:> "OperationId") <*> (pure (fromEnum s)))

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
-- * 'ddtlrOperationId'
--
-- * 'ddtlrStatus'
data DisableDomainTransferLockResponse = DisableDomainTransferLockResponse'
    { _ddtlrOperationId :: !Text
    , _ddtlrStatus      :: !Int
    } deriving (Eq,Read,Show)

-- | 'DisableDomainTransferLockResponse' smart constructor.
disableDomainTransferLockResponse :: Text -> Int -> DisableDomainTransferLockResponse
disableDomainTransferLockResponse pOperationId pStatus =
    DisableDomainTransferLockResponse'
    { _ddtlrOperationId = pOperationId
    , _ddtlrStatus = pStatus
    }

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
ddtlrOperationId :: Lens' DisableDomainTransferLockResponse Text
ddtlrOperationId = lens _ddtlrOperationId (\ s a -> s{_ddtlrOperationId = a});

-- | FIXME: Undocumented member.
ddtlrStatus :: Lens' DisableDomainTransferLockResponse Int
ddtlrStatus = lens _ddtlrStatus (\ s a -> s{_ddtlrStatus = a});
