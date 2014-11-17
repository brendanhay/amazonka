{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53Domains.UpdateDomainNameservers
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation replaces the current set of name servers for the domain with
-- the specified set of name servers. If you use Amazon Route 53 as your DNS
-- service, specify the four name servers in the delegation set for the hosted
-- zone for the domain. If successful, this operation returns an operation ID
-- that you can use to track the progress and completion of the action. If the
-- request is not completed successfully, the domain registrant will be
-- notified by email.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-UpdateDomainNameservers.html>
module Network.AWS.Route53Domains.UpdateDomainNameservers
    (
    -- * Request
      UpdateDomainNameservers
    -- ** Request constructor
    , updateDomainNameservers
    -- ** Request lenses
    , udnDomainName
    , udnNameservers

    -- * Response
    , UpdateDomainNameserversResponse
    -- ** Response constructor
    , updateDomainNameserversResponse
    -- ** Response lenses
    , udnrOperationId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Route53Domains.Types
import qualified GHC.Exts

data UpdateDomainNameservers = UpdateDomainNameservers
    { _udnDomainName  :: Text
    , _udnNameservers :: [Nameserver]
    } deriving (Eq, Show, Generic)

-- | 'UpdateDomainNameservers' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udnDomainName' @::@ 'Text'
--
-- * 'udnNameservers' @::@ ['Nameserver']
--
updateDomainNameservers :: Text -- ^ 'udnDomainName'
                        -> UpdateDomainNameservers
updateDomainNameservers p1 = UpdateDomainNameservers
    { _udnDomainName  = p1
    , _udnNameservers = mempty
    }

-- | The name of a domain. Type: String Default: None Constraints: The domain
-- name can contain only the letters a through z, the numbers 0 through 9,
-- and hyphen (-). Internationalized Domain Names are not supported.
-- Required: Yes.
udnDomainName :: Lens' UpdateDomainNameservers Text
udnDomainName = lens _udnDomainName (\s a -> s { _udnDomainName = a })

-- | A list of new name servers for the domain. Type: Complex Children: Name,
-- GlueIps Required: Yes.
udnNameservers :: Lens' UpdateDomainNameservers [Nameserver]
udnNameservers = lens _udnNameservers (\s a -> s { _udnNameservers = a })

newtype UpdateDomainNameserversResponse = UpdateDomainNameserversResponse
    { _udnrOperationId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'UpdateDomainNameserversResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'udnrOperationId' @::@ 'Text'
--
updateDomainNameserversResponse :: Text -- ^ 'udnrOperationId'
                                -> UpdateDomainNameserversResponse
updateDomainNameserversResponse p1 = UpdateDomainNameserversResponse
    { _udnrOperationId = p1
    }

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail. Type: String Default:
-- None Constraints: Maximum 255 characters.
udnrOperationId :: Lens' UpdateDomainNameserversResponse Text
udnrOperationId = lens _udnrOperationId (\s a -> s { _udnrOperationId = a })

instance ToPath UpdateDomainNameservers where
    toPath = const "/"

instance ToQuery UpdateDomainNameservers where
    toQuery = const mempty

instance ToHeaders UpdateDomainNameservers
instance ToJSON UpdateDomainNameservers where
    toJSON = genericToJSON jsonOptions

instance AWSRequest UpdateDomainNameservers where
    type Sv UpdateDomainNameservers = Route53Domains
    type Rs UpdateDomainNameservers = UpdateDomainNameserversResponse

    request  = post "UpdateDomainNameservers"
    response = jsonResponse

instance FromJSON UpdateDomainNameserversResponse where
    parseJSON = genericParseJSON jsonOptions
