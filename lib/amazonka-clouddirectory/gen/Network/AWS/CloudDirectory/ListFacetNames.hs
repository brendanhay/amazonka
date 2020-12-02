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
-- Module      : Network.AWS.CloudDirectory.ListFacetNames
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the names of facets that exist in a schema.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListFacetNames
    (
    -- * Creating a Request
      listFacetNames
    , ListFacetNames
    -- * Request Lenses
    , lfnNextToken
    , lfnMaxResults
    , lfnSchemaARN

    -- * Destructuring the Response
    , listFacetNamesResponse
    , ListFacetNamesResponse
    -- * Response Lenses
    , lfnrsNextToken
    , lfnrsFacetNames
    , lfnrsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listFacetNames' smart constructor.
data ListFacetNames = ListFacetNames'
  { _lfnNextToken  :: !(Maybe Text)
  , _lfnMaxResults :: !(Maybe Nat)
  , _lfnSchemaARN  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListFacetNames' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfnNextToken' - The pagination token.
--
-- * 'lfnMaxResults' - The maximum number of results to retrieve.
--
-- * 'lfnSchemaARN' - The Amazon Resource Name (ARN) to retrieve facet names from.
listFacetNames
    :: Text -- ^ 'lfnSchemaARN'
    -> ListFacetNames
listFacetNames pSchemaARN_ =
  ListFacetNames'
    { _lfnNextToken = Nothing
    , _lfnMaxResults = Nothing
    , _lfnSchemaARN = pSchemaARN_
    }


-- | The pagination token.
lfnNextToken :: Lens' ListFacetNames (Maybe Text)
lfnNextToken = lens _lfnNextToken (\ s a -> s{_lfnNextToken = a})

-- | The maximum number of results to retrieve.
lfnMaxResults :: Lens' ListFacetNames (Maybe Natural)
lfnMaxResults = lens _lfnMaxResults (\ s a -> s{_lfnMaxResults = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) to retrieve facet names from.
lfnSchemaARN :: Lens' ListFacetNames Text
lfnSchemaARN = lens _lfnSchemaARN (\ s a -> s{_lfnSchemaARN = a})

instance AWSPager ListFacetNames where
        page rq rs
          | stop (rs ^. lfnrsNextToken) = Nothing
          | stop (rs ^. lfnrsFacetNames) = Nothing
          | otherwise =
            Just $ rq & lfnNextToken .~ rs ^. lfnrsNextToken

instance AWSRequest ListFacetNames where
        type Rs ListFacetNames = ListFacetNamesResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 ListFacetNamesResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "FacetNames" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListFacetNames where

instance NFData ListFacetNames where

instance ToHeaders ListFacetNames where
        toHeaders ListFacetNames'{..}
          = mconcat ["x-amz-data-partition" =# _lfnSchemaARN]

instance ToJSON ListFacetNames where
        toJSON ListFacetNames'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lfnNextToken,
                  ("MaxResults" .=) <$> _lfnMaxResults])

instance ToPath ListFacetNames where
        toPath
          = const "/amazonclouddirectory/2017-01-11/facet/list"

instance ToQuery ListFacetNames where
        toQuery = const mempty

-- | /See:/ 'listFacetNamesResponse' smart constructor.
data ListFacetNamesResponse = ListFacetNamesResponse'
  { _lfnrsNextToken      :: !(Maybe Text)
  , _lfnrsFacetNames     :: !(Maybe [Text])
  , _lfnrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListFacetNamesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfnrsNextToken' - The pagination token.
--
-- * 'lfnrsFacetNames' - The names of facets that exist within the schema.
--
-- * 'lfnrsResponseStatus' - -- | The response status code.
listFacetNamesResponse
    :: Int -- ^ 'lfnrsResponseStatus'
    -> ListFacetNamesResponse
listFacetNamesResponse pResponseStatus_ =
  ListFacetNamesResponse'
    { _lfnrsNextToken = Nothing
    , _lfnrsFacetNames = Nothing
    , _lfnrsResponseStatus = pResponseStatus_
    }


-- | The pagination token.
lfnrsNextToken :: Lens' ListFacetNamesResponse (Maybe Text)
lfnrsNextToken = lens _lfnrsNextToken (\ s a -> s{_lfnrsNextToken = a})

-- | The names of facets that exist within the schema.
lfnrsFacetNames :: Lens' ListFacetNamesResponse [Text]
lfnrsFacetNames = lens _lfnrsFacetNames (\ s a -> s{_lfnrsFacetNames = a}) . _Default . _Coerce

-- | -- | The response status code.
lfnrsResponseStatus :: Lens' ListFacetNamesResponse Int
lfnrsResponseStatus = lens _lfnrsResponseStatus (\ s a -> s{_lfnrsResponseStatus = a})

instance NFData ListFacetNamesResponse where
