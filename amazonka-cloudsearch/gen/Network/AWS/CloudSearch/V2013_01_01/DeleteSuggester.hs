{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.DeleteSuggester
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a suggester. For more information, see Getting Search Suggestions
-- in the Amazon CloudSearch Developer Guide.
module Network.AWS.CloudSearch.V2013_01_01.DeleteSuggester
    (
    -- * Request
      DeleteSuggester
    -- ** Request constructor
    , deleteSuggester
    -- ** Request lenses
    , dstDomainName
    , dstSuggesterName

    -- * Response
    , DeleteSuggesterResponse
    -- ** Response lenses
    , dsuSuggester
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteSuggester' request.
deleteSuggester :: Text -- ^ 'dstDomainName'
                -> Text -- ^ 'dstSuggesterName'
                -> DeleteSuggester
deleteSuggester p1 p2 = DeleteSuggester
    { _dstDomainName = p1
    , _dstSuggesterName = p2
    }
{-# INLINE deleteSuggester #-}

data DeleteSuggester = DeleteSuggester
    { _dstDomainName :: Text
      -- ^ A string that represents the name of a domain. Domain names are
      -- unique across the domains owned by an account within an AWS
      -- region. Domain names start with a letter or number and can
      -- contain the following characters: a-z (lowercase), 0-9, and -
      -- (hyphen).
    , _dstSuggesterName :: Text
      -- ^ Specifies the name of the suggester you want to delete.
    } deriving (Show, Generic)

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
dstDomainName :: Lens' DeleteSuggester (Text)
dstDomainName f x =
    f (_dstDomainName x)
        <&> \y -> x { _dstDomainName = y }
{-# INLINE dstDomainName #-}

-- | Specifies the name of the suggester you want to delete.
dstSuggesterName :: Lens' DeleteSuggester (Text)
dstSuggesterName f x =
    f (_dstSuggesterName x)
        <&> \y -> x { _dstSuggesterName = y }
{-# INLINE dstSuggesterName #-}

instance ToQuery DeleteSuggester where
    toQuery = genericQuery def

data DeleteSuggesterResponse = DeleteSuggesterResponse
    { _dsuSuggester :: SuggesterStatus
      -- ^ The status of the suggester being deleted.
    } deriving (Show, Generic)

-- | The status of the suggester being deleted.
dsuSuggester :: Lens' DeleteSuggesterResponse (SuggesterStatus)
dsuSuggester f x =
    f (_dsuSuggester x)
        <&> \y -> x { _dsuSuggester = y }
{-# INLINE dsuSuggester #-}

instance FromXML DeleteSuggesterResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteSuggester where
    type Sv DeleteSuggester = CloudSearch
    type Rs DeleteSuggester = DeleteSuggesterResponse

    request = post "DeleteSuggester"
    response _ = xmlResponse
