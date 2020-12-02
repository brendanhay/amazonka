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
-- Module      : Network.AWS.SSM.DescribeInventoryDeletions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a specific delete inventory operation.
--
--
module Network.AWS.SSM.DescribeInventoryDeletions
    (
    -- * Creating a Request
      describeInventoryDeletions
    , DescribeInventoryDeletions
    -- * Request Lenses
    , didNextToken
    , didMaxResults
    , didDeletionId

    -- * Destructuring the Response
    , describeInventoryDeletionsResponse
    , DescribeInventoryDeletionsResponse
    -- * Response Lenses
    , didrsInventoryDeletions
    , didrsNextToken
    , didrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'describeInventoryDeletions' smart constructor.
data DescribeInventoryDeletions = DescribeInventoryDeletions'
  { _didNextToken  :: !(Maybe Text)
  , _didMaxResults :: !(Maybe Nat)
  , _didDeletionId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInventoryDeletions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'didNextToken' - A token to start the list. Use this token to get the next set of results.
--
-- * 'didMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- * 'didDeletionId' - Specify the delete inventory ID for which you want information. This ID was returned by the @DeleteInventory@ action.
describeInventoryDeletions
    :: DescribeInventoryDeletions
describeInventoryDeletions =
  DescribeInventoryDeletions'
    { _didNextToken = Nothing
    , _didMaxResults = Nothing
    , _didDeletionId = Nothing
    }


-- | A token to start the list. Use this token to get the next set of results.
didNextToken :: Lens' DescribeInventoryDeletions (Maybe Text)
didNextToken = lens _didNextToken (\ s a -> s{_didNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
didMaxResults :: Lens' DescribeInventoryDeletions (Maybe Natural)
didMaxResults = lens _didMaxResults (\ s a -> s{_didMaxResults = a}) . mapping _Nat

-- | Specify the delete inventory ID for which you want information. This ID was returned by the @DeleteInventory@ action.
didDeletionId :: Lens' DescribeInventoryDeletions (Maybe Text)
didDeletionId = lens _didDeletionId (\ s a -> s{_didDeletionId = a})

instance AWSRequest DescribeInventoryDeletions where
        type Rs DescribeInventoryDeletions =
             DescribeInventoryDeletionsResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribeInventoryDeletionsResponse' <$>
                   (x .?> "InventoryDeletions" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeInventoryDeletions where

instance NFData DescribeInventoryDeletions where

instance ToHeaders DescribeInventoryDeletions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribeInventoryDeletions" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeInventoryDeletions where
        toJSON DescribeInventoryDeletions'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _didNextToken,
                  ("MaxResults" .=) <$> _didMaxResults,
                  ("DeletionId" .=) <$> _didDeletionId])

instance ToPath DescribeInventoryDeletions where
        toPath = const "/"

instance ToQuery DescribeInventoryDeletions where
        toQuery = const mempty

-- | /See:/ 'describeInventoryDeletionsResponse' smart constructor.
data DescribeInventoryDeletionsResponse = DescribeInventoryDeletionsResponse'
  { _didrsInventoryDeletions :: !(Maybe [InventoryDeletionStatusItem])
  , _didrsNextToken          :: !(Maybe Text)
  , _didrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInventoryDeletionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'didrsInventoryDeletions' - A list of status items for deleted inventory.
--
-- * 'didrsNextToken' - The token for the next set of items to return. Use this token to get the next set of results.
--
-- * 'didrsResponseStatus' - -- | The response status code.
describeInventoryDeletionsResponse
    :: Int -- ^ 'didrsResponseStatus'
    -> DescribeInventoryDeletionsResponse
describeInventoryDeletionsResponse pResponseStatus_ =
  DescribeInventoryDeletionsResponse'
    { _didrsInventoryDeletions = Nothing
    , _didrsNextToken = Nothing
    , _didrsResponseStatus = pResponseStatus_
    }


-- | A list of status items for deleted inventory.
didrsInventoryDeletions :: Lens' DescribeInventoryDeletionsResponse [InventoryDeletionStatusItem]
didrsInventoryDeletions = lens _didrsInventoryDeletions (\ s a -> s{_didrsInventoryDeletions = a}) . _Default . _Coerce

-- | The token for the next set of items to return. Use this token to get the next set of results.
didrsNextToken :: Lens' DescribeInventoryDeletionsResponse (Maybe Text)
didrsNextToken = lens _didrsNextToken (\ s a -> s{_didrsNextToken = a})

-- | -- | The response status code.
didrsResponseStatus :: Lens' DescribeInventoryDeletionsResponse Int
didrsResponseStatus = lens _didrsResponseStatus (\ s a -> s{_didrsResponseStatus = a})

instance NFData DescribeInventoryDeletionsResponse
         where
