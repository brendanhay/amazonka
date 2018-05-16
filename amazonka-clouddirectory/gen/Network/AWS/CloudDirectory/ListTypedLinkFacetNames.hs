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
-- Module      : Network.AWS.CloudDirectory.ListTypedLinkFacetNames
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of @TypedLink@ facet names for a particular schema. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
--
--
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListTypedLinkFacetNames
    (
    -- * Creating a Request
      listTypedLinkFacetNames
    , ListTypedLinkFacetNames
    -- * Request Lenses
    , ltlfnNextToken
    , ltlfnMaxResults
    , ltlfnSchemaARN

    -- * Destructuring the Response
    , listTypedLinkFacetNamesResponse
    , ListTypedLinkFacetNamesResponse
    -- * Response Lenses
    , ltlfnrsNextToken
    , ltlfnrsFacetNames
    , ltlfnrsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTypedLinkFacetNames' smart constructor.
data ListTypedLinkFacetNames = ListTypedLinkFacetNames'
  { _ltlfnNextToken  :: !(Maybe Text)
  , _ltlfnMaxResults :: !(Maybe Nat)
  , _ltlfnSchemaARN  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTypedLinkFacetNames' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltlfnNextToken' - The pagination token.
--
-- * 'ltlfnMaxResults' - The maximum number of results to retrieve.
--
-- * 'ltlfnSchemaARN' - The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
listTypedLinkFacetNames
    :: Text -- ^ 'ltlfnSchemaARN'
    -> ListTypedLinkFacetNames
listTypedLinkFacetNames pSchemaARN_ =
  ListTypedLinkFacetNames'
    { _ltlfnNextToken = Nothing
    , _ltlfnMaxResults = Nothing
    , _ltlfnSchemaARN = pSchemaARN_
    }


-- | The pagination token.
ltlfnNextToken :: Lens' ListTypedLinkFacetNames (Maybe Text)
ltlfnNextToken = lens _ltlfnNextToken (\ s a -> s{_ltlfnNextToken = a})

-- | The maximum number of results to retrieve.
ltlfnMaxResults :: Lens' ListTypedLinkFacetNames (Maybe Natural)
ltlfnMaxResults = lens _ltlfnMaxResults (\ s a -> s{_ltlfnMaxResults = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
ltlfnSchemaARN :: Lens' ListTypedLinkFacetNames Text
ltlfnSchemaARN = lens _ltlfnSchemaARN (\ s a -> s{_ltlfnSchemaARN = a})

instance AWSPager ListTypedLinkFacetNames where
        page rq rs
          | stop (rs ^. ltlfnrsNextToken) = Nothing
          | stop (rs ^. ltlfnrsFacetNames) = Nothing
          | otherwise =
            Just $ rq & ltlfnNextToken .~ rs ^. ltlfnrsNextToken

instance AWSRequest ListTypedLinkFacetNames where
        type Rs ListTypedLinkFacetNames =
             ListTypedLinkFacetNamesResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 ListTypedLinkFacetNamesResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "FacetNames" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListTypedLinkFacetNames where

instance NFData ListTypedLinkFacetNames where

instance ToHeaders ListTypedLinkFacetNames where
        toHeaders ListTypedLinkFacetNames'{..}
          = mconcat ["x-amz-data-partition" =# _ltlfnSchemaARN]

instance ToJSON ListTypedLinkFacetNames where
        toJSON ListTypedLinkFacetNames'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _ltlfnNextToken,
                  ("MaxResults" .=) <$> _ltlfnMaxResults])

instance ToPath ListTypedLinkFacetNames where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/typedlink/facet/list"

instance ToQuery ListTypedLinkFacetNames where
        toQuery = const mempty

-- | /See:/ 'listTypedLinkFacetNamesResponse' smart constructor.
data ListTypedLinkFacetNamesResponse = ListTypedLinkFacetNamesResponse'
  { _ltlfnrsNextToken      :: !(Maybe Text)
  , _ltlfnrsFacetNames     :: !(Maybe [Text])
  , _ltlfnrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTypedLinkFacetNamesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltlfnrsNextToken' - The pagination token.
--
-- * 'ltlfnrsFacetNames' - The names of typed link facets that exist within the schema.
--
-- * 'ltlfnrsResponseStatus' - -- | The response status code.
listTypedLinkFacetNamesResponse
    :: Int -- ^ 'ltlfnrsResponseStatus'
    -> ListTypedLinkFacetNamesResponse
listTypedLinkFacetNamesResponse pResponseStatus_ =
  ListTypedLinkFacetNamesResponse'
    { _ltlfnrsNextToken = Nothing
    , _ltlfnrsFacetNames = Nothing
    , _ltlfnrsResponseStatus = pResponseStatus_
    }


-- | The pagination token.
ltlfnrsNextToken :: Lens' ListTypedLinkFacetNamesResponse (Maybe Text)
ltlfnrsNextToken = lens _ltlfnrsNextToken (\ s a -> s{_ltlfnrsNextToken = a})

-- | The names of typed link facets that exist within the schema.
ltlfnrsFacetNames :: Lens' ListTypedLinkFacetNamesResponse [Text]
ltlfnrsFacetNames = lens _ltlfnrsFacetNames (\ s a -> s{_ltlfnrsFacetNames = a}) . _Default . _Coerce

-- | -- | The response status code.
ltlfnrsResponseStatus :: Lens' ListTypedLinkFacetNamesResponse Int
ltlfnrsResponseStatus = lens _ltlfnrsResponseStatus (\ s a -> s{_ltlfnrsResponseStatus = a})

instance NFData ListTypedLinkFacetNamesResponse where
