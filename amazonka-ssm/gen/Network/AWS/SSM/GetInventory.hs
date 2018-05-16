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
-- Module      : Network.AWS.SSM.GetInventory
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Query inventory information.
--
--
module Network.AWS.SSM.GetInventory
    (
    -- * Creating a Request
      getInventory
    , GetInventory
    -- * Request Lenses
    , giAggregators
    , giFilters
    , giResultAttributes
    , giNextToken
    , giMaxResults

    -- * Destructuring the Response
    , getInventoryResponse
    , GetInventoryResponse
    -- * Response Lenses
    , girsEntities
    , girsNextToken
    , girsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'getInventory' smart constructor.
data GetInventory = GetInventory'
  { _giAggregators      :: !(Maybe (List1 InventoryAggregator))
  , _giFilters          :: !(Maybe (List1 InventoryFilter))
  , _giResultAttributes :: !(Maybe (List1 ResultAttribute))
  , _giNextToken        :: !(Maybe Text)
  , _giMaxResults       :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInventory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'giAggregators' - Returns counts of inventory types based on one or more expressions. For example, if you aggregate by using an expression that uses the @AWS:InstanceInformation.PlatformType@ type, you can see a count of how many Windows and Linux instances exist in your inventoried fleet.
--
-- * 'giFilters' - One or more filters. Use a filter to return a more specific list of results.
--
-- * 'giResultAttributes' - The list of inventory item types to return.
--
-- * 'giNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'giMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
getInventory
    :: GetInventory
getInventory =
  GetInventory'
    { _giAggregators = Nothing
    , _giFilters = Nothing
    , _giResultAttributes = Nothing
    , _giNextToken = Nothing
    , _giMaxResults = Nothing
    }


-- | Returns counts of inventory types based on one or more expressions. For example, if you aggregate by using an expression that uses the @AWS:InstanceInformation.PlatformType@ type, you can see a count of how many Windows and Linux instances exist in your inventoried fleet.
giAggregators :: Lens' GetInventory (Maybe (NonEmpty InventoryAggregator))
giAggregators = lens _giAggregators (\ s a -> s{_giAggregators = a}) . mapping _List1

-- | One or more filters. Use a filter to return a more specific list of results.
giFilters :: Lens' GetInventory (Maybe (NonEmpty InventoryFilter))
giFilters = lens _giFilters (\ s a -> s{_giFilters = a}) . mapping _List1

-- | The list of inventory item types to return.
giResultAttributes :: Lens' GetInventory (Maybe (NonEmpty ResultAttribute))
giResultAttributes = lens _giResultAttributes (\ s a -> s{_giResultAttributes = a}) . mapping _List1

-- | The token for the next set of items to return. (You received this token from a previous call.)
giNextToken :: Lens' GetInventory (Maybe Text)
giNextToken = lens _giNextToken (\ s a -> s{_giNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
giMaxResults :: Lens' GetInventory (Maybe Natural)
giMaxResults = lens _giMaxResults (\ s a -> s{_giMaxResults = a}) . mapping _Nat

instance AWSRequest GetInventory where
        type Rs GetInventory = GetInventoryResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 GetInventoryResponse' <$>
                   (x .?> "Entities" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable GetInventory where

instance NFData GetInventory where

instance ToHeaders GetInventory where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.GetInventory" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetInventory where
        toJSON GetInventory'{..}
          = object
              (catMaybes
                 [("Aggregators" .=) <$> _giAggregators,
                  ("Filters" .=) <$> _giFilters,
                  ("ResultAttributes" .=) <$> _giResultAttributes,
                  ("NextToken" .=) <$> _giNextToken,
                  ("MaxResults" .=) <$> _giMaxResults])

instance ToPath GetInventory where
        toPath = const "/"

instance ToQuery GetInventory where
        toQuery = const mempty

-- | /See:/ 'getInventoryResponse' smart constructor.
data GetInventoryResponse = GetInventoryResponse'
  { _girsEntities       :: !(Maybe [InventoryResultEntity])
  , _girsNextToken      :: !(Maybe Text)
  , _girsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInventoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'girsEntities' - Collection of inventory entities such as a collection of instance inventory.
--
-- * 'girsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'girsResponseStatus' - -- | The response status code.
getInventoryResponse
    :: Int -- ^ 'girsResponseStatus'
    -> GetInventoryResponse
getInventoryResponse pResponseStatus_ =
  GetInventoryResponse'
    { _girsEntities = Nothing
    , _girsNextToken = Nothing
    , _girsResponseStatus = pResponseStatus_
    }


-- | Collection of inventory entities such as a collection of instance inventory.
girsEntities :: Lens' GetInventoryResponse [InventoryResultEntity]
girsEntities = lens _girsEntities (\ s a -> s{_girsEntities = a}) . _Default . _Coerce

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
girsNextToken :: Lens' GetInventoryResponse (Maybe Text)
girsNextToken = lens _girsNextToken (\ s a -> s{_girsNextToken = a})

-- | -- | The response status code.
girsResponseStatus :: Lens' GetInventoryResponse Int
girsResponseStatus = lens _girsResponseStatus (\ s a -> s{_girsResponseStatus = a})

instance NFData GetInventoryResponse where
