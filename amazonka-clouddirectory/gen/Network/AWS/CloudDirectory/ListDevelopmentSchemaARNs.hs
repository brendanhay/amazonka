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
-- Module      : Network.AWS.CloudDirectory.ListDevelopmentSchemaARNs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves each Amazon Resource Name (ARN) of schemas in the development state.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListDevelopmentSchemaARNs
    (
    -- * Creating a Request
      listDevelopmentSchemaARNs
    , ListDevelopmentSchemaARNs
    -- * Request Lenses
    , ldsaNextToken
    , ldsaMaxResults

    -- * Destructuring the Response
    , listDevelopmentSchemaARNsResponse
    , ListDevelopmentSchemaARNsResponse
    -- * Response Lenses
    , ldsarsSchemaARNs
    , ldsarsNextToken
    , ldsarsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDevelopmentSchemaARNs' smart constructor.
data ListDevelopmentSchemaARNs = ListDevelopmentSchemaARNs'
  { _ldsaNextToken  :: !(Maybe Text)
  , _ldsaMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDevelopmentSchemaARNs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldsaNextToken' - The pagination token.
--
-- * 'ldsaMaxResults' - The maximum number of results to retrieve.
listDevelopmentSchemaARNs
    :: ListDevelopmentSchemaARNs
listDevelopmentSchemaARNs =
  ListDevelopmentSchemaARNs'
    {_ldsaNextToken = Nothing, _ldsaMaxResults = Nothing}


-- | The pagination token.
ldsaNextToken :: Lens' ListDevelopmentSchemaARNs (Maybe Text)
ldsaNextToken = lens _ldsaNextToken (\ s a -> s{_ldsaNextToken = a})

-- | The maximum number of results to retrieve.
ldsaMaxResults :: Lens' ListDevelopmentSchemaARNs (Maybe Natural)
ldsaMaxResults = lens _ldsaMaxResults (\ s a -> s{_ldsaMaxResults = a}) . mapping _Nat

instance AWSPager ListDevelopmentSchemaARNs where
        page rq rs
          | stop (rs ^. ldsarsNextToken) = Nothing
          | stop (rs ^. ldsarsSchemaARNs) = Nothing
          | otherwise =
            Just $ rq & ldsaNextToken .~ rs ^. ldsarsNextToken

instance AWSRequest ListDevelopmentSchemaARNs where
        type Rs ListDevelopmentSchemaARNs =
             ListDevelopmentSchemaARNsResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 ListDevelopmentSchemaARNsResponse' <$>
                   (x .?> "SchemaArns" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListDevelopmentSchemaARNs where

instance NFData ListDevelopmentSchemaARNs where

instance ToHeaders ListDevelopmentSchemaARNs where
        toHeaders = const mempty

instance ToJSON ListDevelopmentSchemaARNs where
        toJSON ListDevelopmentSchemaARNs'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _ldsaNextToken,
                  ("MaxResults" .=) <$> _ldsaMaxResults])

instance ToPath ListDevelopmentSchemaARNs where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/schema/development"

instance ToQuery ListDevelopmentSchemaARNs where
        toQuery = const mempty

-- | /See:/ 'listDevelopmentSchemaARNsResponse' smart constructor.
data ListDevelopmentSchemaARNsResponse = ListDevelopmentSchemaARNsResponse'
  { _ldsarsSchemaARNs     :: !(Maybe [Text])
  , _ldsarsNextToken      :: !(Maybe Text)
  , _ldsarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDevelopmentSchemaARNsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldsarsSchemaARNs' - The ARNs of retrieved development schemas.
--
-- * 'ldsarsNextToken' - The pagination token.
--
-- * 'ldsarsResponseStatus' - -- | The response status code.
listDevelopmentSchemaARNsResponse
    :: Int -- ^ 'ldsarsResponseStatus'
    -> ListDevelopmentSchemaARNsResponse
listDevelopmentSchemaARNsResponse pResponseStatus_ =
  ListDevelopmentSchemaARNsResponse'
    { _ldsarsSchemaARNs = Nothing
    , _ldsarsNextToken = Nothing
    , _ldsarsResponseStatus = pResponseStatus_
    }


-- | The ARNs of retrieved development schemas.
ldsarsSchemaARNs :: Lens' ListDevelopmentSchemaARNsResponse [Text]
ldsarsSchemaARNs = lens _ldsarsSchemaARNs (\ s a -> s{_ldsarsSchemaARNs = a}) . _Default . _Coerce

-- | The pagination token.
ldsarsNextToken :: Lens' ListDevelopmentSchemaARNsResponse (Maybe Text)
ldsarsNextToken = lens _ldsarsNextToken (\ s a -> s{_ldsarsNextToken = a})

-- | -- | The response status code.
ldsarsResponseStatus :: Lens' ListDevelopmentSchemaARNsResponse Int
ldsarsResponseStatus = lens _ldsarsResponseStatus (\ s a -> s{_ldsarsResponseStatus = a})

instance NFData ListDevelopmentSchemaARNsResponse
         where
