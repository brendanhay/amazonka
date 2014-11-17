{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53Domains.DisableDomainTransferLock
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation removes the transfer lock on the domain (specifically the
-- clientTransferProhibited status) to allow domain transfers. We recommend
-- you refrain from performing this action unless you intend to transfer the
-- domain to a different registrar. Successful submission returns an operation
-- ID that you can use to track the progress and completion of the action. If
-- the request is not completed successfully, the domain registrant will be
-- notified by email.
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
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Route53Domains.Types
import qualified GHC.Exts

newtype DisableDomainTransferLock = DisableDomainTransferLock
    { _ddtlDomainName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DisableDomainTransferLock' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddtlDomainName' @::@ 'Text'
--
disableDomainTransferLock :: Text -- ^ 'ddtlDomainName'
                          -> DisableDomainTransferLock
disableDomainTransferLock p1 = DisableDomainTransferLock
    { _ddtlDomainName = p1
    }

-- | The name of a domain. Type: String Default: None Constraints: The domain
-- name can contain only the letters a through z, the numbers 0 through 9,
-- and hyphen (-). Internationalized Domain Names are not supported.
-- Required: Yes.
ddtlDomainName :: Lens' DisableDomainTransferLock Text
ddtlDomainName = lens _ddtlDomainName (\s a -> s { _ddtlDomainName = a })

newtype DisableDomainTransferLockResponse = DisableDomainTransferLockResponse
    { _ddtlrOperationId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DisableDomainTransferLockResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddtlrOperationId' @::@ 'Text'
--
disableDomainTransferLockResponse :: Text -- ^ 'ddtlrOperationId'
                                  -> DisableDomainTransferLockResponse
disableDomainTransferLockResponse p1 = DisableDomainTransferLockResponse
    { _ddtlrOperationId = p1
    }

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail. Type: String Default:
-- None Constraints: Maximum 255 characters.
ddtlrOperationId :: Lens' DisableDomainTransferLockResponse Text
ddtlrOperationId = lens _ddtlrOperationId (\s a -> s { _ddtlrOperationId = a })

instance AWSRequest DisableDomainTransferLock where
    type Sv DisableDomainTransferLock = Route53Domains
    type Rs DisableDomainTransferLock = DisableDomainTransferLockResponse

    request  = post
    response = jsonResponse

instance FromJSON DisableDomainTransferLockResponse where
    parseJSON = genericParseJSON jsonOptions

instance ToPath DisableDomainTransferLock where
    toPath = const "/"

instance ToHeaders DisableDomainTransferLock

instance ToQuery DisableDomainTransferLock where
    toQuery = const mempty

instance ToJSON DisableDomainTransferLock where
    toJSON = genericToJSON jsonOptions
