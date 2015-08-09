{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Select
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @Select@ operation returns a set of attributes for @ItemNames@ that
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
-- /See:/ <http://docs.aws.amazon.com/AmazonSimpleDB/latest/DeveloperGuide/SDB_API_Select.html AWS API Reference> for Select.
module Network.AWS.SDB.Select
    (
    -- * Creating a Request
      Select
    , select
    -- * Request Lenses
    , sConsistentRead
    , sNextToken
    , sSelectExpression

    -- * Destructuring the Response
    , SelectResponse
    , selectResponse
    -- * Response Lenses
    , srsItems
    , srsNextToken
    , srsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SDB.Types
import           Network.AWS.SDB.Types.Product

-- | /See:/ 'select' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sConsistentRead'
--
-- * 'sNextToken'
--
-- * 'sSelectExpression'
data Select = Select'
    { _sConsistentRead   :: !(Maybe Bool)
    , _sNextToken        :: !(Maybe Text)
    , _sSelectExpression :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Select' smart constructor.
select :: Text -> Select
select pSelectExpression_ =
    Select'
    { _sConsistentRead = Nothing
    , _sNextToken = Nothing
    , _sSelectExpression = pSelectExpression_
    }

-- | Determines whether or not strong consistency should be enforced when
-- data is read from SimpleDB. If @true@, any data previously written to
-- SimpleDB will be returned. Otherwise, results will be consistent
-- eventually, and the client may not see data that was written immediately
-- before your read.
sConsistentRead :: Lens' Select (Maybe Bool)
sConsistentRead = lens _sConsistentRead (\ s a -> s{_sConsistentRead = a});

-- | A string informing Amazon SimpleDB where to start the next list of
-- @ItemNames@.
sNextToken :: Lens' Select (Maybe Text)
sNextToken = lens _sNextToken (\ s a -> s{_sNextToken = a});

-- | The expression used to query the domain.
sSelectExpression :: Lens' Select Text
sSelectExpression = lens _sSelectExpression (\ s a -> s{_sSelectExpression = a});

instance AWSPager Select where
        page rq rs
          | stop (rs ^. srsNextToken) = Nothing
          | stop (rs ^. srsItems) = Nothing
          | otherwise =
            Just $ rq & sNextToken .~ rs ^. srsNextToken

instance AWSRequest Select where
        type Sv Select = SDB
        type Rs Select = SelectResponse
        request = postQuery
        response
          = receiveXMLWrapper "SelectResult"
              (\ s h x ->
                 SelectResponse' <$>
                   (may (parseXMLList "Item") x) <*> (x .@? "NextToken")
                     <*> (pure (fromEnum s)))

instance ToHeaders Select where
        toHeaders = const mempty

instance ToPath Select where
        toPath = const "/"

instance ToQuery Select where
        toQuery Select'{..}
          = mconcat
              ["Action" =: ("Select" :: ByteString),
               "Version" =: ("2009-04-15" :: ByteString),
               "ConsistentRead" =: _sConsistentRead,
               "NextToken" =: _sNextToken,
               "SelectExpression" =: _sSelectExpression]

-- | /See:/ 'selectResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srsItems'
--
-- * 'srsNextToken'
--
-- * 'srsStatus'
data SelectResponse = SelectResponse'
    { _srsItems     :: !(Maybe [Item])
    , _srsNextToken :: !(Maybe Text)
    , _srsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SelectResponse' smart constructor.
selectResponse :: Int -> SelectResponse
selectResponse pStatus_ =
    SelectResponse'
    { _srsItems = Nothing
    , _srsNextToken = Nothing
    , _srsStatus = pStatus_
    }

-- | A list of items that match the select expression.
srsItems :: Lens' SelectResponse [Item]
srsItems = lens _srsItems (\ s a -> s{_srsItems = a}) . _Default . _Coerce;

-- | An opaque token indicating that more items than @MaxNumberOfItems@ were
-- matched, the response size exceeded 1 megabyte, or the execution time
-- exceeded 5 seconds.
srsNextToken :: Lens' SelectResponse (Maybe Text)
srsNextToken = lens _srsNextToken (\ s a -> s{_srsNextToken = a});

-- | Undocumented member.
srsStatus :: Lens' SelectResponse Int
srsStatus = lens _srsStatus (\ s a -> s{_srsStatus = a});
