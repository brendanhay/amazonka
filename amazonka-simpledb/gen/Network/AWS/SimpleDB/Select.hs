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

-- Module      : Network.AWS.SimpleDB.Select
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
module Network.AWS.SimpleDB.Select
    (
    -- * Request
      Select
    -- ** Request constructor
    , select
    -- ** Request lenses
    , sConsistentRead
    , sNextToken
    , sSelectExpression

    -- * Response
    , SelectResult
    -- ** Response constructor
    , selectResult
    -- ** Response lenses
    , srItems
    , srNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SimpleDB.Types

data Select = Select
    { _sConsistentRead   :: Maybe Bool
    , _sNextToken        :: Maybe Text
    , _sSelectExpression :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Select' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sConsistentRead' @::@ 'Maybe' 'Bool'
--
-- * 'sNextToken' @::@ 'Maybe' 'Text'
--
-- * 'sSelectExpression' @::@ 'Text'
--
select :: Text -- ^ 'sSelectExpression'
       -> Select
select p1 = Select
    { _sSelectExpression = p1
    , _sNextToken        = Nothing
    , _sConsistentRead   = Nothing
    }

-- | Determines whether or not strong consistency should be enforced when data
-- is read from SimpleDB. If true, any data previously written to SimpleDB
-- will be returned. Otherwise, results will be consistent eventually, and
-- the client may not see data that was written immediately before your
-- read.
sConsistentRead :: Lens' Select (Maybe Bool)
sConsistentRead = lens _sConsistentRead (\s a -> s { _sConsistentRead = a })

-- | A string informing Amazon SimpleDB where to start the next list of
-- ItemNames.
sNextToken :: Lens' Select (Maybe Text)
sNextToken = lens _sNextToken (\s a -> s { _sNextToken = a })

-- | The expression used to query the domain.
sSelectExpression :: Lens' Select Text
sSelectExpression =
    lens _sSelectExpression (\s a -> s { _sSelectExpression = a })
instance ToQuery Select

instance ToPath Select where
    toPath = const "/"

data SelectResult = SelectResult
    { _srItems     :: [Item]
    , _srNextToken :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'SelectResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srItems' @::@ ['Item']
--
-- * 'srNextToken' @::@ 'Maybe' 'Text'
--
selectResult :: SelectResult
selectResult = SelectResult
    { _srItems     = mempty
    , _srNextToken = Nothing
    }

-- | A list of items that match the select expression.
srItems :: Lens' SelectResult [Item]
srItems = lens _srItems (\s a -> s { _srItems = a })

-- | An opaque token indicating that more items than MaxNumberOfItems were
-- matched, the response size exceeded 1 megabyte, or the execution time
-- exceeded 5 seconds.
srNextToken :: Lens' SelectResult (Maybe Text)
srNextToken = lens _srNextToken (\s a -> s { _srNextToken = a })
instance FromXML SelectResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SelectResult"

instance AWSRequest Select where
    type Sv Select = SimpleDB
    type Rs Select = SelectResult

    request  = post "Select"
    response = xmlResponse $ \h x -> SelectResult
        <$> x %| "Items"
        <*> x %| "NextToken"
