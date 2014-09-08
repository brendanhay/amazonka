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
    , mkDeleteSuggester
    -- ** Request lenses
    , ds2DomainName
    , ds2SuggesterName

    -- * Response
    , DeleteSuggesterResponse
    -- ** Response constructor
    , mkDeleteSuggesterResponse
    -- ** Response lenses
    , dsrrSuggester
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Container for the parameters to the DeleteSuggester operation. Specifies
-- the name of the domain you want to update and name of the suggester you
-- want to delete.
data DeleteSuggester = DeleteSuggester
    { _ds2DomainName :: Text
    , _ds2SuggesterName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteSuggester' request.
mkDeleteSuggester :: Text -- ^ 'ds2DomainName'
                  -> Text -- ^ 'ds2SuggesterName'
                  -> DeleteSuggester
mkDeleteSuggester p1 p2 = DeleteSuggester
    { _ds2DomainName = p1
    , _ds2SuggesterName = p2
    }

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
ds2DomainName :: Lens' DeleteSuggester Text
ds2DomainName = lens _ds2DomainName (\s a -> s { _ds2DomainName = a })

-- | Specifies the name of the suggester you want to delete.
ds2SuggesterName :: Lens' DeleteSuggester Text
ds2SuggesterName =
    lens _ds2SuggesterName (\s a -> s { _ds2SuggesterName = a })

instance ToQuery DeleteSuggester where
    toQuery = genericQuery def

-- | The result of a DeleteSuggester request. Contains the status of the deleted
-- suggester.
newtype DeleteSuggesterResponse = DeleteSuggesterResponse
    { _dsrrSuggester :: SuggesterStatus
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteSuggesterResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteSuggesterResponse :: SuggesterStatus -- ^ 'dsrrSuggester'
                          -> DeleteSuggesterResponse
mkDeleteSuggesterResponse p1 = DeleteSuggesterResponse
    { _dsrrSuggester = p1
    }

-- | The status of the suggester being deleted.
dsrrSuggester :: Lens' DeleteSuggesterResponse SuggesterStatus
dsrrSuggester = lens _dsrrSuggester (\s a -> s { _dsrrSuggester = a })

instance FromXML DeleteSuggesterResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteSuggester where
    type Sv DeleteSuggester = CloudSearch
    type Rs DeleteSuggester = DeleteSuggesterResponse

    request = post "DeleteSuggester"
    response _ = xmlResponse
