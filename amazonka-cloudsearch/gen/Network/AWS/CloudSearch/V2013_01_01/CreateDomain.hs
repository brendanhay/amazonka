{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.CreateDomain
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new search domain. For more information, see Creating a Search
-- Domain in the Amazon CloudSearch Developer Guide.
module Network.AWS.CloudSearch.V2013_01_01.CreateDomain
    (
    -- * Request
      CreateDomain
    -- ** Request constructor
    , mkCreateDomainRequest
    -- ** Request lenses
    , cdrDomainName

    -- * Response
    , CreateDomainResponse
    -- ** Response lenses
    , cdsDomainStatus
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateDomain' request.
mkCreateDomainRequest :: Text -- ^ 'cdrDomainName'
                      -> CreateDomain
mkCreateDomainRequest p1 = CreateDomain
    { _cdrDomainName = p1
    }
{-# INLINE mkCreateDomainRequest #-}

newtype CreateDomain = CreateDomain
    { _cdrDomainName :: Text
      -- ^ A name for the domain you are creating. Allowed characters are
      -- a-z (lower-case letters), 0-9, and hyphen (-). Domain names must
      -- start with a letter or number and be at least 3 and no more than
      -- 28 characters long.
    } deriving (Show, Generic)

-- | A name for the domain you are creating. Allowed characters are a-z
-- (lower-case letters), 0-9, and hyphen (-). Domain names must start with a
-- letter or number and be at least 3 and no more than 28 characters long.
cdrDomainName :: Lens' CreateDomain (Text)
cdrDomainName = lens _cdrDomainName (\s a -> s { _cdrDomainName = a })
{-# INLINE cdrDomainName #-}

instance ToQuery CreateDomain where
    toQuery = genericQuery def

newtype CreateDomainResponse = CreateDomainResponse
    { _cdsDomainStatus :: Maybe DomainStatus
      -- ^ The current status of the search domain.
    } deriving (Show, Generic)

-- | The current status of the search domain.
cdsDomainStatus :: Lens' CreateDomainResponse (Maybe DomainStatus)
cdsDomainStatus = lens _cdsDomainStatus (\s a -> s { _cdsDomainStatus = a })
{-# INLINE cdsDomainStatus #-}

instance FromXML CreateDomainResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateDomain where
    type Sv CreateDomain = CloudSearch
    type Rs CreateDomain = CreateDomainResponse

    request = post "CreateDomain"
    response _ = xmlResponse
