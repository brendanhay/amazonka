{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SDB.Select
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The 'Select' operation returns a set of attributes for 'ItemNames' that match
-- the select expression. 'Select' is similar to the standard SQL SELECT
-- statement.
--
-- The total size of the response cannot exceed 1 MB in total size. Amazon
-- SimpleDB automatically adjusts the number of items returned per page to
-- enforce this limit. For example, if the client asks to retrieve 2500 items,
-- but each individual item is 10 kB in size, the system returns 100 items and
-- an appropriate 'NextToken' so the client can access the next page of results.
--
-- For information on how to construct select expressions, see Using Select to
-- Create Amazon SimpleDB Queries in the Developer Guide.
--
-- <http://docs.aws.amazon.com/AmazonSimpleDB/latest/DeveloperGuide/SDB_API_Select.html>
module Network.AWS.SDB.Select
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
    , SelectResponse
    -- ** Response constructor
    , selectResponse
    -- ** Response lenses
    , srItems
    , srNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SDB.Types
import qualified GHC.Exts

data Select = Select
    { _sConsistentRead   :: Maybe Bool
    , _sNextToken        :: Maybe Text
    , _sSelectExpression :: Text
    } deriving (Eq, Ord, Read, Show)

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

-- | Determines whether or not strong consistency should be enforced when data is
-- read from SimpleDB. If 'true', any data previously written to SimpleDB will be
-- returned. Otherwise, results will be consistent eventually, and the client
-- may not see data that was written immediately before your read.
sConsistentRead :: Lens' Select (Maybe Bool)
sConsistentRead = lens _sConsistentRead (\s a -> s { _sConsistentRead = a })

-- | A string informing Amazon SimpleDB where to start the next list of 'ItemNames'.
sNextToken :: Lens' Select (Maybe Text)
sNextToken = lens _sNextToken (\s a -> s { _sNextToken = a })

-- | The expression used to query the domain.
sSelectExpression :: Lens' Select Text
sSelectExpression =
    lens _sSelectExpression (\s a -> s { _sSelectExpression = a })

data SelectResponse = SelectResponse
    { _srItems     :: List "member" Item
    , _srNextToken :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'SelectResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srItems' @::@ ['Item']
--
-- * 'srNextToken' @::@ 'Maybe' 'Text'
--
selectResponse :: SelectResponse
selectResponse = SelectResponse
    { _srItems     = mempty
    , _srNextToken = Nothing
    }

-- | A list of items that match the select expression.
srItems :: Lens' SelectResponse [Item]
srItems = lens _srItems (\s a -> s { _srItems = a }) . _List

-- | An opaque token indicating that more items than 'MaxNumberOfItems' were
-- matched, the response size exceeded 1 megabyte, or the execution time
-- exceeded 5 seconds.
srNextToken :: Lens' SelectResponse (Maybe Text)
srNextToken = lens _srNextToken (\s a -> s { _srNextToken = a })

instance ToPath Select where
    toPath = const "/"

instance ToQuery Select where
    toQuery Select{..} = mconcat
        [ "ConsistentRead"   =? _sConsistentRead
        , "NextToken"        =? _sNextToken
        , "SelectExpression" =? _sSelectExpression
        ]

instance ToHeaders Select

instance AWSRequest Select where
    type Sv Select = SDB
    type Rs Select = SelectResponse

    request  = post "Select"
    response = xmlResponse

instance FromXML SelectResponse where
    parseXML = withElement "SelectResult" $ \x -> SelectResponse
        <$> parseXML x
        <*> x .@? "NextToken"

instance AWSPager Select where
    page rq rs
        | stop (rs ^. srNextToken) = Nothing
        | otherwise = (\x -> rq & sNextToken ?~ x)
            <$> (rs ^. srNextToken)
