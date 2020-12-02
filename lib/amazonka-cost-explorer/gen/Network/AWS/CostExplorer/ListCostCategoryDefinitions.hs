{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.ListCostCategoryDefinitions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the name, ARN, @NumberOfRules@ and effective dates of all Cost Categories defined in the account. You have the option to use @EffectiveOn@ to return a list of Cost Categories that were active on a specific date. If there is no @EffectiveOn@ specified, you’ll see Cost Categories that are effective on the current date. If Cost Category is still effective, @EffectiveEnd@ is omitted in the response. @ListCostCategoryDefinitions@ supports pagination. The request can have a @MaxResults@ range up to 100.
module Network.AWS.CostExplorer.ListCostCategoryDefinitions
  ( -- * Creating a Request
    listCostCategoryDefinitions,
    ListCostCategoryDefinitions,

    -- * Request Lenses
    lccdEffectiveOn,
    lccdNextToken,
    lccdMaxResults,

    -- * Destructuring the Response
    listCostCategoryDefinitionsResponse,
    ListCostCategoryDefinitionsResponse,

    -- * Response Lenses
    lccdrsCostCategoryReferences,
    lccdrsNextToken,
    lccdrsResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listCostCategoryDefinitions' smart constructor.
data ListCostCategoryDefinitions = ListCostCategoryDefinitions'
  { _lccdEffectiveOn ::
      !(Maybe Text),
    _lccdNextToken :: !(Maybe Text),
    _lccdMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListCostCategoryDefinitions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lccdEffectiveOn' - The date when the Cost Category was effective.
--
-- * 'lccdNextToken' - The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'lccdMaxResults' - The number of entries a paginated response contains.
listCostCategoryDefinitions ::
  ListCostCategoryDefinitions
listCostCategoryDefinitions =
  ListCostCategoryDefinitions'
    { _lccdEffectiveOn = Nothing,
      _lccdNextToken = Nothing,
      _lccdMaxResults = Nothing
    }

-- | The date when the Cost Category was effective.
lccdEffectiveOn :: Lens' ListCostCategoryDefinitions (Maybe Text)
lccdEffectiveOn = lens _lccdEffectiveOn (\s a -> s {_lccdEffectiveOn = a})

-- | The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
lccdNextToken :: Lens' ListCostCategoryDefinitions (Maybe Text)
lccdNextToken = lens _lccdNextToken (\s a -> s {_lccdNextToken = a})

-- | The number of entries a paginated response contains.
lccdMaxResults :: Lens' ListCostCategoryDefinitions (Maybe Natural)
lccdMaxResults = lens _lccdMaxResults (\s a -> s {_lccdMaxResults = a}) . mapping _Nat

instance AWSRequest ListCostCategoryDefinitions where
  type
    Rs ListCostCategoryDefinitions =
      ListCostCategoryDefinitionsResponse
  request = postJSON costExplorer
  response =
    receiveJSON
      ( \s h x ->
          ListCostCategoryDefinitionsResponse'
            <$> (x .?> "CostCategoryReferences" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListCostCategoryDefinitions

instance NFData ListCostCategoryDefinitions

instance ToHeaders ListCostCategoryDefinitions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSInsightsIndexService.ListCostCategoryDefinitions" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListCostCategoryDefinitions where
  toJSON ListCostCategoryDefinitions' {..} =
    object
      ( catMaybes
          [ ("EffectiveOn" .=) <$> _lccdEffectiveOn,
            ("NextToken" .=) <$> _lccdNextToken,
            ("MaxResults" .=) <$> _lccdMaxResults
          ]
      )

instance ToPath ListCostCategoryDefinitions where
  toPath = const "/"

instance ToQuery ListCostCategoryDefinitions where
  toQuery = const mempty

-- | /See:/ 'listCostCategoryDefinitionsResponse' smart constructor.
data ListCostCategoryDefinitionsResponse = ListCostCategoryDefinitionsResponse'
  { _lccdrsCostCategoryReferences ::
      !( Maybe
           [CostCategoryReference]
       ),
    _lccdrsNextToken ::
      !(Maybe Text),
    _lccdrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListCostCategoryDefinitionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lccdrsCostCategoryReferences' - A reference to a Cost Category containing enough information to identify the Cost Category.
--
-- * 'lccdrsNextToken' - The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
--
-- * 'lccdrsResponseStatus' - -- | The response status code.
listCostCategoryDefinitionsResponse ::
  -- | 'lccdrsResponseStatus'
  Int ->
  ListCostCategoryDefinitionsResponse
listCostCategoryDefinitionsResponse pResponseStatus_ =
  ListCostCategoryDefinitionsResponse'
    { _lccdrsCostCategoryReferences =
        Nothing,
      _lccdrsNextToken = Nothing,
      _lccdrsResponseStatus = pResponseStatus_
    }

-- | A reference to a Cost Category containing enough information to identify the Cost Category.
lccdrsCostCategoryReferences :: Lens' ListCostCategoryDefinitionsResponse [CostCategoryReference]
lccdrsCostCategoryReferences = lens _lccdrsCostCategoryReferences (\s a -> s {_lccdrsCostCategoryReferences = a}) . _Default . _Coerce

-- | The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
lccdrsNextToken :: Lens' ListCostCategoryDefinitionsResponse (Maybe Text)
lccdrsNextToken = lens _lccdrsNextToken (\s a -> s {_lccdrsNextToken = a})

-- | -- | The response status code.
lccdrsResponseStatus :: Lens' ListCostCategoryDefinitionsResponse Int
lccdrsResponseStatus = lens _lccdrsResponseStatus (\s a -> s {_lccdrsResponseStatus = a})

instance NFData ListCostCategoryDefinitionsResponse
