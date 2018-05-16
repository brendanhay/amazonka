{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Select
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @Select@ operation returns a set of attributes for @ItemNames@ that match the select expression. @Select@ is similar to the standard SQL SELECT statement.
--
--
-- The total size of the response cannot exceed 1 MB in total size. Amazon SimpleDB automatically adjusts the number of items returned per page to enforce this limit. For example, if the client asks to retrieve 2500 items, but each individual item is 10 kB in size, the system returns 100 items and an appropriate @NextToken@ so the client can access the next page of results.
--
-- For information on how to construct select expressions, see Using Select to Create Amazon SimpleDB Queries in the Developer Guide.
--
--
-- This operation returns paginated results.
module Network.AWS.SDB.Select
    (
    -- * Creating a Request
      select
    , Select
    -- * Request Lenses
    , sConsistentRead
    , sNextToken
    , sSelectExpression

    -- * Destructuring the Response
    , selectResponse
    , SelectResponse
    -- * Response Lenses
    , srsItems
    , srsNextToken
    , srsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SDB.Types
import Network.AWS.SDB.Types.Product

-- | /See:/ 'select' smart constructor.
data Select = Select'
  { _sConsistentRead   :: !(Maybe Bool)
  , _sNextToken        :: !(Maybe Text)
  , _sSelectExpression :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Select' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sConsistentRead' - @true@
--
-- * 'sNextToken' - @ItemNames@
--
-- * 'sSelectExpression' - The expression used to query the domain.
select
    :: Text -- ^ 'sSelectExpression'
    -> Select
select pSelectExpression_ =
  Select'
    { _sConsistentRead = Nothing
    , _sNextToken = Nothing
    , _sSelectExpression = pSelectExpression_
    }


-- | @true@
sConsistentRead :: Lens' Select (Maybe Bool)
sConsistentRead = lens _sConsistentRead (\ s a -> s{_sConsistentRead = a})

-- | @ItemNames@
sNextToken :: Lens' Select (Maybe Text)
sNextToken = lens _sNextToken (\ s a -> s{_sNextToken = a})

-- | The expression used to query the domain.
sSelectExpression :: Lens' Select Text
sSelectExpression = lens _sSelectExpression (\ s a -> s{_sSelectExpression = a})

instance AWSPager Select where
        page rq rs
          | stop (rs ^. srsNextToken) = Nothing
          | stop (rs ^. srsItems) = Nothing
          | otherwise =
            Just $ rq & sNextToken .~ rs ^. srsNextToken

instance AWSRequest Select where
        type Rs Select = SelectResponse
        request = postQuery sdb
        response
          = receiveXMLWrapper "SelectResult"
              (\ s h x ->
                 SelectResponse' <$>
                   (may (parseXMLList "Item") x) <*> (x .@? "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable Select where

instance NFData Select where

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
data SelectResponse = SelectResponse'
  { _srsItems          :: !(Maybe [Item])
  , _srsNextToken      :: !(Maybe Text)
  , _srsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SelectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsItems' - A list of items that match the select expression.
--
-- * 'srsNextToken' - @MaxNumberOfItems@
--
-- * 'srsResponseStatus' - -- | The response status code.
selectResponse
    :: Int -- ^ 'srsResponseStatus'
    -> SelectResponse
selectResponse pResponseStatus_ =
  SelectResponse'
    { _srsItems = Nothing
    , _srsNextToken = Nothing
    , _srsResponseStatus = pResponseStatus_
    }


-- | A list of items that match the select expression.
srsItems :: Lens' SelectResponse [Item]
srsItems = lens _srsItems (\ s a -> s{_srsItems = a}) . _Default . _Coerce

-- | @MaxNumberOfItems@
srsNextToken :: Lens' SelectResponse (Maybe Text)
srsNextToken = lens _srsNextToken (\ s a -> s{_srsNextToken = a})

-- | -- | The response status code.
srsResponseStatus :: Lens' SelectResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

instance NFData SelectResponse where
