{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

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

-- | The @Select@ operation returns a set of attributes for @ItemNames@ that
-- match the select expression. @Select@ is similar to the standard SQL
-- SELECT statement.
--
-- The total size of the response cannot exceed 1 MB in total size. Amazon
-- SimpleDB automatically adjusts the number of items returned per page to
-- enforce this limit. For example, if the client asks to retrieve 2500
-- items, but each individual item is 10 kB in size, the system returns 100
-- items and an appropriate @NextToken@ so the client can access the next
-- page of results.
--
-- For information on how to construct select expressions, see Using Select
-- to Create Amazon SimpleDB Queries in the Developer Guide.
--
-- <http://docs.aws.amazon.com/AmazonSimpleDB/latest/DeveloperGuide/SDB_API_Select.html>
module Network.AWS.SDB.Select
    (
    -- * Request
      Select
    -- ** Request constructor
    , select
    -- ** Request lenses
    , selConsistentRead
    , selNextToken
    , selSelectExpression

    -- * Response
    , SelectResponse
    -- ** Response constructor
    , selectResponse
    -- ** Response lenses
    , srItems
    , srNextToken
    , srStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SDB.Types

-- | /See:/ 'select' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'selConsistentRead'
--
-- * 'selNextToken'
--
-- * 'selSelectExpression'
data Select = Select'
    { _selConsistentRead   :: !(Maybe Bool)
    , _selNextToken        :: !(Maybe Text)
    , _selSelectExpression :: !Text
    } deriving (Eq,Read,Show)

-- | 'Select' smart constructor.
select :: Text -> Select
select pSelectExpression =
    Select'
    { _selConsistentRead = Nothing
    , _selNextToken = Nothing
    , _selSelectExpression = pSelectExpression
    }

-- | Determines whether or not strong consistency should be enforced when
-- data is read from SimpleDB. If @true@, any data previously written to
-- SimpleDB will be returned. Otherwise, results will be consistent
-- eventually, and the client may not see data that was written immediately
-- before your read.
selConsistentRead :: Lens' Select (Maybe Bool)
selConsistentRead = lens _selConsistentRead (\ s a -> s{_selConsistentRead = a});

-- | A string informing Amazon SimpleDB where to start the next list of
-- @ItemNames@.
selNextToken :: Lens' Select (Maybe Text)
selNextToken = lens _selNextToken (\ s a -> s{_selNextToken = a});

-- | The expression used to query the domain.
selSelectExpression :: Lens' Select Text
selSelectExpression = lens _selSelectExpression (\ s a -> s{_selSelectExpression = a});

instance AWSPager Select where
        page rq rs
          | stop (rs ^. srNextToken) = Nothing
          | stop (rs ^. srItems) = Nothing
          | otherwise =
            Just $ rq & selNextToken .~ rs ^. srNextToken

instance AWSRequest Select where
        type Sv Select = SDB
        type Rs Select = SelectResponse
        request = post
        response
          = receiveXMLWrapper "SelectResult"
              (\ s h x ->
                 SelectResponse' <$>
                   (may (parseXMLList "Item") x) <*> (x .@? "NextToken")
                     <*> (pure s))

instance ToHeaders Select where
        toHeaders = const mempty

instance ToPath Select where
        toPath = const "/"

instance ToQuery Select where
        toQuery Select'{..}
          = mconcat
              ["Action" =: ("Select" :: ByteString),
               "Version" =: ("2009-04-15" :: ByteString),
               "ConsistentRead" =: _selConsistentRead,
               "NextToken" =: _selNextToken,
               "SelectExpression" =: _selSelectExpression]

-- | /See:/ 'selectResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srItems'
--
-- * 'srNextToken'
--
-- * 'srStatus'
data SelectResponse = SelectResponse'
    { _srItems     :: !(Maybe [Item])
    , _srNextToken :: !(Maybe Text)
    , _srStatus    :: !Status
    } deriving (Eq,Read,Show)

-- | 'SelectResponse' smart constructor.
selectResponse :: Status -> SelectResponse
selectResponse pStatus =
    SelectResponse'
    { _srItems = Nothing
    , _srNextToken = Nothing
    , _srStatus = pStatus
    }

-- | A list of items that match the select expression.
srItems :: Lens' SelectResponse [Item]
srItems = lens _srItems (\ s a -> s{_srItems = a}) . _Default;

-- | An opaque token indicating that more items than @MaxNumberOfItems@ were
-- matched, the response size exceeded 1 megabyte, or the execution time
-- exceeded 5 seconds.
srNextToken :: Lens' SelectResponse (Maybe Text)
srNextToken = lens _srNextToken (\ s a -> s{_srNextToken = a});

-- | FIXME: Undocumented member.
srStatus :: Lens' SelectResponse Status
srStatus = lens _srStatus (\ s a -> s{_srStatus = a});
