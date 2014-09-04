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
    , mkSelectRequest
    -- ** Request lenses
    , sstSelectExpression
    , sstNextToken
    , sstConsistentRead

    -- * Response
    , SelectResponse
    -- ** Response lenses
    , ssuItems
    , ssuNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.SimpleDB.V2009_04_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'Select' request.
mkSelectRequest :: Text -- ^ 'sstSelectExpression'
                -> Select
mkSelectRequest p1 = Select
    { _sstSelectExpression = p1
    , _sstNextToken = Nothing
    , _sstConsistentRead = Nothing
    }
{-# INLINE mkSelectRequest #-}

data Select = Select
    { _sstSelectExpression :: Text
      -- ^ The expression used to query the domain.
    , _sstNextToken :: Maybe Text
      -- ^ A string informing Amazon SimpleDB where to start the next list
      -- of ItemNames.
    , _sstConsistentRead :: Maybe Bool
      -- ^ Determines whether or not strong consistency should be enforced
      -- when data is read from SimpleDB. If true, any data previously
      -- written to SimpleDB will be returned. Otherwise, results will be
      -- consistent eventually, and the client may not see data that was
      -- written immediately before your read.
    } deriving (Show, Generic)

-- | The expression used to query the domain.
sstSelectExpression :: Lens' Select (Text)
sstSelectExpression = lens _sstSelectExpression (\s a -> s { _sstSelectExpression = a })
{-# INLINE sstSelectExpression #-}

-- | A string informing Amazon SimpleDB where to start the next list of
-- ItemNames.
sstNextToken :: Lens' Select (Maybe Text)
sstNextToken = lens _sstNextToken (\s a -> s { _sstNextToken = a })
{-# INLINE sstNextToken #-}

-- | Determines whether or not strong consistency should be enforced when data
-- is read from SimpleDB. If true, any data previously written to SimpleDB
-- will be returned. Otherwise, results will be consistent eventually, and the
-- client may not see data that was written immediately before your read.
sstConsistentRead :: Lens' Select (Maybe Bool)
sstConsistentRead = lens _sstConsistentRead (\s a -> s { _sstConsistentRead = a })
{-# INLINE sstConsistentRead #-}

instance ToQuery Select where
    toQuery = genericQuery def

data SelectResponse = SelectResponse
    { _ssuItems :: [Item]
      -- ^ A list of items that match the select expression.
    , _ssuNextToken :: Maybe Text
      -- ^ An opaque token indicating that more items than MaxNumberOfItems
      -- were matched, the response size exceeded 1 megabyte, or the
      -- execution time exceeded 5 seconds.
    } deriving (Show, Generic)

-- | A list of items that match the select expression.
ssuItems :: Lens' SelectResponse ([Item])
ssuItems = lens _ssuItems (\s a -> s { _ssuItems = a })
{-# INLINE ssuItems #-}

-- | An opaque token indicating that more items than MaxNumberOfItems were
-- matched, the response size exceeded 1 megabyte, or the execution time
-- exceeded 5 seconds.
ssuNextToken :: Lens' SelectResponse (Maybe Text)
ssuNextToken = lens _ssuNextToken (\s a -> s { _ssuNextToken = a })
{-# INLINE ssuNextToken #-}

instance FromXML SelectResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest Select where
    type Sv Select = SimpleDB
    type Rs Select = SelectResponse

    request = post "Select"
    response _ = xmlResponse

instance AWSPager Select where
    next rq rs = (\x -> rq { _sstNextToken = Just x })
        <$> (_ssuNextToken rs)
