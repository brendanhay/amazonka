{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.Route53Domains.EnableDomainTransferLock
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation sets the transfer lock on the domain (specifically the
-- clientTransferProhibited status) to prevent domain transfers. Successful
-- submission returns an operation ID that you can use to track the progress
-- and completion of the action. If the request is not completed successfully,
-- the domain registrant will be notified by email.
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
    , edtlrOperationId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Route53Domains.Types

newtype EnableDomainTransferLock = EnableDomainTransferLock
    { _edtlDomainName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'EnableDomainTransferLock' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'edtlDomainName' @::@ 'Text'
--
enableDomainTransferLock :: Text -- ^ 'edtlDomainName'
                         -> EnableDomainTransferLock
enableDomainTransferLock p1 = EnableDomainTransferLock
    { _edtlDomainName = p1
    }

-- | The name of a domain. Type: String Default: None Constraints: The domain
-- name can contain only the letters a through z, the numbers 0 through 9,
-- and hyphen (-). Internationalized Domain Names are not supported.
-- Required: Yes.
edtlDomainName :: Lens' EnableDomainTransferLock Text
edtlDomainName = lens _edtlDomainName (\s a -> s { _edtlDomainName = a })

instance ToPath EnableDomainTransferLock where
    toPath = const "/"

instance ToQuery EnableDomainTransferLock where
    toQuery = const mempty

instance ToHeaders EnableDomainTransferLock

instance ToBody EnableDomainTransferLock where
    toBody = toBody . encode . _edtlDomainName

newtype EnableDomainTransferLockResponse = EnableDomainTransferLockResponse
    { _edtlrOperationId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'EnableDomainTransferLockResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'edtlrOperationId' @::@ 'Text'
--
enableDomainTransferLockResponse :: Text -- ^ 'edtlrOperationId'
                                 -> EnableDomainTransferLockResponse
enableDomainTransferLockResponse p1 = EnableDomainTransferLockResponse
    { _edtlrOperationId = p1
    }

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail. Type: String Default:
-- None Constraints: Maximum 255 characters.
edtlrOperationId :: Lens' EnableDomainTransferLockResponse Text
edtlrOperationId = lens _edtlrOperationId (\s a -> s { _edtlrOperationId = a })

instance AWSRequest EnableDomainTransferLock where
    type Sv EnableDomainTransferLock = Route53Domains
    type Rs EnableDomainTransferLock = EnableDomainTransferLockResponse

    request  = post
    response = jsonResponse $ \h o -> EnableDomainTransferLockResponse
        <$> o .: "OperationId"
