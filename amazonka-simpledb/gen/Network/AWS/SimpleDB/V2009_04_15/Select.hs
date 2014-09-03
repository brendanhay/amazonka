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
    , select
    -- ** Request lenses
    , sstSelectExpression
    , sstConsistentRead
    , sstNextToken

    -- * Response
    , SelectResponse
    -- ** Response lenses
    , ssuItems
    , ssuNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.SimpleDB.V2009_04_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'Select' request.
select :: Text -- ^ 'sstSelectExpression'
       -> Select
select p1 = Select
    { _sstSelectExpression = p1
    , _sstConsistentRead = Nothing
    , _sstNextToken = Nothing
    }

data Select = Select
    { _sstSelectExpression :: Text
      -- ^ The expression used to query the domain.
    , _sstConsistentRead :: Maybe Bool
      -- ^ Determines whether or not strong consistency should be enforced
      -- when data is read from SimpleDB. If true, any data previously
      -- written to SimpleDB will be returned. Otherwise, results will be
      -- consistent eventually, and the client may not see data that was
      -- written immediately before your read.
    , _sstNextToken :: Maybe Text
      -- ^ A string informing Amazon SimpleDB where to start the next list
      -- of ItemNames.
    } deriving (Show, Generic)

-- | The expression used to query the domain.
sstSelectExpression
    :: Functor f
    => (Text
    -> f (Text))
    -> Select
    -> f Select
sstSelectExpression f x =
    (\y -> x { _sstSelectExpression = y })
       <$> f (_sstSelectExpression x)
{-# INLINE sstSelectExpression #-}

-- | Determines whether or not strong consistency should be enforced when data
-- is read from SimpleDB. If true, any data previously written to SimpleDB
-- will be returned. Otherwise, results will be consistent eventually, and the
-- client may not see data that was written immediately before your read.
sstConsistentRead
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Select
    -> f Select
sstConsistentRead f x =
    (\y -> x { _sstConsistentRead = y })
       <$> f (_sstConsistentRead x)
{-# INLINE sstConsistentRead #-}

-- | A string informing Amazon SimpleDB where to start the next list of
-- ItemNames.
sstNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Select
    -> f Select
sstNextToken f x =
    (\y -> x { _sstNextToken = y })
       <$> f (_sstNextToken x)
{-# INLINE sstNextToken #-}

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
ssuItems
    :: Functor f
    => ([Item]
    -> f ([Item]))
    -> SelectResponse
    -> f SelectResponse
ssuItems f x =
    (\y -> x { _ssuItems = y })
       <$> f (_ssuItems x)
{-# INLINE ssuItems #-}

-- | An opaque token indicating that more items than MaxNumberOfItems were
-- matched, the response size exceeded 1 megabyte, or the execution time
-- exceeded 5 seconds.
ssuNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SelectResponse
    -> f SelectResponse
ssuNextToken f x =
    (\y -> x { _ssuNextToken = y })
       <$> f (_ssuNextToken x)
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
