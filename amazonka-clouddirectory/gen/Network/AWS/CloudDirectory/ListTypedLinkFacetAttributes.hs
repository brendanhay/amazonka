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
-- Module      : Network.AWS.CloudDirectory.ListTypedLinkFacetAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all attribute definitions for a particular 'TypedLinkFacet' . For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
--
--
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListTypedLinkFacetAttributes
    (
    -- * Creating a Request
      listTypedLinkFacetAttributes
    , ListTypedLinkFacetAttributes
    -- * Request Lenses
    , ltlfaNextToken
    , ltlfaMaxResults
    , ltlfaSchemaARN
    , ltlfaName

    -- * Destructuring the Response
    , listTypedLinkFacetAttributesResponse
    , ListTypedLinkFacetAttributesResponse
    -- * Response Lenses
    , ltlfarsNextToken
    , ltlfarsAttributes
    , ltlfarsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTypedLinkFacetAttributes' smart constructor.
data ListTypedLinkFacetAttributes = ListTypedLinkFacetAttributes'
  { _ltlfaNextToken  :: !(Maybe Text)
  , _ltlfaMaxResults :: !(Maybe Nat)
  , _ltlfaSchemaARN  :: !Text
  , _ltlfaName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTypedLinkFacetAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltlfaNextToken' - The pagination token.
--
-- * 'ltlfaMaxResults' - The maximum number of results to retrieve.
--
-- * 'ltlfaSchemaARN' - The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
--
-- * 'ltlfaName' - The unique name of the typed link facet.
listTypedLinkFacetAttributes
    :: Text -- ^ 'ltlfaSchemaARN'
    -> Text -- ^ 'ltlfaName'
    -> ListTypedLinkFacetAttributes
listTypedLinkFacetAttributes pSchemaARN_ pName_ =
  ListTypedLinkFacetAttributes'
    { _ltlfaNextToken = Nothing
    , _ltlfaMaxResults = Nothing
    , _ltlfaSchemaARN = pSchemaARN_
    , _ltlfaName = pName_
    }


-- | The pagination token.
ltlfaNextToken :: Lens' ListTypedLinkFacetAttributes (Maybe Text)
ltlfaNextToken = lens _ltlfaNextToken (\ s a -> s{_ltlfaNextToken = a})

-- | The maximum number of results to retrieve.
ltlfaMaxResults :: Lens' ListTypedLinkFacetAttributes (Maybe Natural)
ltlfaMaxResults = lens _ltlfaMaxResults (\ s a -> s{_ltlfaMaxResults = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
ltlfaSchemaARN :: Lens' ListTypedLinkFacetAttributes Text
ltlfaSchemaARN = lens _ltlfaSchemaARN (\ s a -> s{_ltlfaSchemaARN = a})

-- | The unique name of the typed link facet.
ltlfaName :: Lens' ListTypedLinkFacetAttributes Text
ltlfaName = lens _ltlfaName (\ s a -> s{_ltlfaName = a})

instance AWSPager ListTypedLinkFacetAttributes where
        page rq rs
          | stop (rs ^. ltlfarsNextToken) = Nothing
          | stop (rs ^. ltlfarsAttributes) = Nothing
          | otherwise =
            Just $ rq & ltlfaNextToken .~ rs ^. ltlfarsNextToken

instance AWSRequest ListTypedLinkFacetAttributes
         where
        type Rs ListTypedLinkFacetAttributes =
             ListTypedLinkFacetAttributesResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 ListTypedLinkFacetAttributesResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Attributes" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListTypedLinkFacetAttributes where

instance NFData ListTypedLinkFacetAttributes where

instance ToHeaders ListTypedLinkFacetAttributes where
        toHeaders ListTypedLinkFacetAttributes'{..}
          = mconcat ["x-amz-data-partition" =# _ltlfaSchemaARN]

instance ToJSON ListTypedLinkFacetAttributes where
        toJSON ListTypedLinkFacetAttributes'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _ltlfaNextToken,
                  ("MaxResults" .=) <$> _ltlfaMaxResults,
                  Just ("Name" .= _ltlfaName)])

instance ToPath ListTypedLinkFacetAttributes where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/typedlink/facet/attributes"

instance ToQuery ListTypedLinkFacetAttributes where
        toQuery = const mempty

-- | /See:/ 'listTypedLinkFacetAttributesResponse' smart constructor.
data ListTypedLinkFacetAttributesResponse = ListTypedLinkFacetAttributesResponse'
  { _ltlfarsNextToken      :: !(Maybe Text)
  , _ltlfarsAttributes     :: !(Maybe [TypedLinkAttributeDefinition])
  , _ltlfarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTypedLinkFacetAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltlfarsNextToken' - The pagination token.
--
-- * 'ltlfarsAttributes' - An ordered set of attributes associate with the typed link.
--
-- * 'ltlfarsResponseStatus' - -- | The response status code.
listTypedLinkFacetAttributesResponse
    :: Int -- ^ 'ltlfarsResponseStatus'
    -> ListTypedLinkFacetAttributesResponse
listTypedLinkFacetAttributesResponse pResponseStatus_ =
  ListTypedLinkFacetAttributesResponse'
    { _ltlfarsNextToken = Nothing
    , _ltlfarsAttributes = Nothing
    , _ltlfarsResponseStatus = pResponseStatus_
    }


-- | The pagination token.
ltlfarsNextToken :: Lens' ListTypedLinkFacetAttributesResponse (Maybe Text)
ltlfarsNextToken = lens _ltlfarsNextToken (\ s a -> s{_ltlfarsNextToken = a})

-- | An ordered set of attributes associate with the typed link.
ltlfarsAttributes :: Lens' ListTypedLinkFacetAttributesResponse [TypedLinkAttributeDefinition]
ltlfarsAttributes = lens _ltlfarsAttributes (\ s a -> s{_ltlfarsAttributes = a}) . _Default . _Coerce

-- | -- | The response status code.
ltlfarsResponseStatus :: Lens' ListTypedLinkFacetAttributesResponse Int
ltlfarsResponseStatus = lens _ltlfarsResponseStatus (\ s a -> s{_ltlfarsResponseStatus = a})

instance NFData ListTypedLinkFacetAttributesResponse
         where
