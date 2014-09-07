{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SimpleDB.V2009_04_15.Select
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The Select operation returns a set of attributes for ItemNames that match
-- the select expression. Select is similar to the standard SQL SELECT
-- statement. The total size of the response cannot exceed 1 MB in total size.
-- Amazon SimpleDB automatically adjusts the number of items returned per page
-- to enforce this limit. For example, if the client asks to retrieve 2500
-- items, but each individual item is 10 kB in size, the system returns 100
-- items and an appropriate NextToken so the client can access the next page
-- of results. For information on how to construct select expressions, see
-- Using Select to Create Amazon SimpleDB Queries in the Developer Guide.
module Network.AWS.SimpleDB.V2009_04_15.Select
    (
    -- * Request
      Select
    -- ** Request constructor
    , mkSelect
    -- ** Request lenses
    , sSelectExpression
    , sNextToken
    , sConsistentRead

    -- * Response
    , SelectResponse
    -- ** Response lenses
    , srsItems
    , srsNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.SimpleDB.V2009_04_15.Types
import Network.AWS.Prelude

data Select = Select
    { _sSelectExpression :: Text
    , _sNextToken :: Maybe Text
    , _sConsistentRead :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'Select' request.
mkSelect :: Text -- ^ 'sSelectExpression'
         -> Select
mkSelect p1 = Select
    { _sSelectExpression = p1
    , _sNextToken = Nothing
    , _sConsistentRead = Nothing
    }

-- | The expression used to query the domain.
sSelectExpression :: Lens' Select Text
sSelectExpression =
    lens _sSelectExpression (\s a -> s { _sSelectExpression = a })

-- | A string informing Amazon SimpleDB where to start the next list of
-- ItemNames.
sNextToken :: Lens' Select (Maybe Text)
sNextToken = lens _sNextToken (\s a -> s { _sNextToken = a })

-- | Determines whether or not strong consistency should be enforced when data
-- is read from SimpleDB. If true, any data previously written to SimpleDB
-- will be returned. Otherwise, results will be consistent eventually, and the
-- client may not see data that was written immediately before your read.
sConsistentRead :: Lens' Select (Maybe Bool)
sConsistentRead = lens _sConsistentRead (\s a -> s { _sConsistentRead = a })

instance ToQuery Select where
    toQuery = genericQuery def

data SelectResponse = SelectResponse
    { _srsItems :: [Item]
    , _srsNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | A list of items that match the select expression.
srsItems :: Lens' SelectResponse [Item]
srsItems = lens _srsItems (\s a -> s { _srsItems = a })

-- | An opaque token indicating that more items than MaxNumberOfItems were
-- matched, the response size exceeded 1 megabyte, or the execution time
-- exceeded 5 seconds.
srsNextToken :: Lens' SelectResponse (Maybe Text)
srsNextToken = lens _srsNextToken (\s a -> s { _srsNextToken = a })

instance FromXML SelectResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest Select where
    type Sv Select = SimpleDB
    type Rs Select = SelectResponse

    request = post "Select"
    response _ = xmlResponse

instance AWSPager Select where
    next rq rs = (\x -> rq & sNextToken ?~ x)
        <$> (rs ^. srsNextToken)
