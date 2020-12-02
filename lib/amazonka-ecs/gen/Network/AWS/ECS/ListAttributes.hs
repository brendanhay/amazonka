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
-- Module      : Network.AWS.ECS.ListAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the attributes for Amazon ECS resources within a specified target type and cluster. When you specify a target type and cluster, @ListAttributes@ returns a list of attribute objects, one for each attribute on each resource. You can filter the list of results to a single attribute name to only return results that have that name. You can also filter the results by attribute name and value, for example, to see which container instances in a cluster are running a Linux AMI (@ecs.os-type=linux@ ).
--
--
module Network.AWS.ECS.ListAttributes
    (
    -- * Creating a Request
      listAttributes
    , ListAttributes
    -- * Request Lenses
    , laAttributeValue
    , laCluster
    , laNextToken
    , laAttributeName
    , laMaxResults
    , laTargetType

    -- * Destructuring the Response
    , listAttributesResponse
    , ListAttributesResponse
    -- * Response Lenses
    , larsNextToken
    , larsAttributes
    , larsResponseStatus
    ) where

import Network.AWS.ECS.Types
import Network.AWS.ECS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAttributes' smart constructor.
data ListAttributes = ListAttributes'
  { _laAttributeValue :: !(Maybe Text)
  , _laCluster        :: !(Maybe Text)
  , _laNextToken      :: !(Maybe Text)
  , _laAttributeName  :: !(Maybe Text)
  , _laMaxResults     :: !(Maybe Int)
  , _laTargetType     :: !TargetType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laAttributeValue' - The value of the attribute with which to filter results. You must also specify an attribute name to use this parameter.
--
-- * 'laCluster' - The short name or full Amazon Resource Name (ARN) of the cluster to list attributes. If you do not specify a cluster, the default cluster is assumed.
--
-- * 'laNextToken' - The @nextToken@ value returned from a previous paginated @ListAttributes@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value.
--
-- * 'laAttributeName' - The name of the attribute with which to filter the results.
--
-- * 'laMaxResults' - The maximum number of cluster results returned by @ListAttributes@ in paginated output. When this parameter is used, @ListAttributes@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListAttributes@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListAttributes@ returns up to 100 results and a @nextToken@ value if applicable.
--
-- * 'laTargetType' - The type of the target with which to list attributes.
listAttributes
    :: TargetType -- ^ 'laTargetType'
    -> ListAttributes
listAttributes pTargetType_ =
  ListAttributes'
    { _laAttributeValue = Nothing
    , _laCluster = Nothing
    , _laNextToken = Nothing
    , _laAttributeName = Nothing
    , _laMaxResults = Nothing
    , _laTargetType = pTargetType_
    }


-- | The value of the attribute with which to filter results. You must also specify an attribute name to use this parameter.
laAttributeValue :: Lens' ListAttributes (Maybe Text)
laAttributeValue = lens _laAttributeValue (\ s a -> s{_laAttributeValue = a})

-- | The short name or full Amazon Resource Name (ARN) of the cluster to list attributes. If you do not specify a cluster, the default cluster is assumed.
laCluster :: Lens' ListAttributes (Maybe Text)
laCluster = lens _laCluster (\ s a -> s{_laCluster = a})

-- | The @nextToken@ value returned from a previous paginated @ListAttributes@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value.
laNextToken :: Lens' ListAttributes (Maybe Text)
laNextToken = lens _laNextToken (\ s a -> s{_laNextToken = a})

-- | The name of the attribute with which to filter the results.
laAttributeName :: Lens' ListAttributes (Maybe Text)
laAttributeName = lens _laAttributeName (\ s a -> s{_laAttributeName = a})

-- | The maximum number of cluster results returned by @ListAttributes@ in paginated output. When this parameter is used, @ListAttributes@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @ListAttributes@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @ListAttributes@ returns up to 100 results and a @nextToken@ value if applicable.
laMaxResults :: Lens' ListAttributes (Maybe Int)
laMaxResults = lens _laMaxResults (\ s a -> s{_laMaxResults = a})

-- | The type of the target with which to list attributes.
laTargetType :: Lens' ListAttributes TargetType
laTargetType = lens _laTargetType (\ s a -> s{_laTargetType = a})

instance AWSRequest ListAttributes where
        type Rs ListAttributes = ListAttributesResponse
        request = postJSON ecs
        response
          = receiveJSON
              (\ s h x ->
                 ListAttributesResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "attributes" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListAttributes where

instance NFData ListAttributes where

instance ToHeaders ListAttributes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.ListAttributes"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListAttributes where
        toJSON ListAttributes'{..}
          = object
              (catMaybes
                 [("attributeValue" .=) <$> _laAttributeValue,
                  ("cluster" .=) <$> _laCluster,
                  ("nextToken" .=) <$> _laNextToken,
                  ("attributeName" .=) <$> _laAttributeName,
                  ("maxResults" .=) <$> _laMaxResults,
                  Just ("targetType" .= _laTargetType)])

instance ToPath ListAttributes where
        toPath = const "/"

instance ToQuery ListAttributes where
        toQuery = const mempty

-- | /See:/ 'listAttributesResponse' smart constructor.
data ListAttributesResponse = ListAttributesResponse'
  { _larsNextToken      :: !(Maybe Text)
  , _larsAttributes     :: !(Maybe [Attribute])
  , _larsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larsNextToken' - The @nextToken@ value to include in a future @ListAttributes@ request. When the results of a @ListAttributes@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'larsAttributes' - A list of attribute objects that meet the criteria of the request.
--
-- * 'larsResponseStatus' - -- | The response status code.
listAttributesResponse
    :: Int -- ^ 'larsResponseStatus'
    -> ListAttributesResponse
listAttributesResponse pResponseStatus_ =
  ListAttributesResponse'
    { _larsNextToken = Nothing
    , _larsAttributes = Nothing
    , _larsResponseStatus = pResponseStatus_
    }


-- | The @nextToken@ value to include in a future @ListAttributes@ request. When the results of a @ListAttributes@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
larsNextToken :: Lens' ListAttributesResponse (Maybe Text)
larsNextToken = lens _larsNextToken (\ s a -> s{_larsNextToken = a})

-- | A list of attribute objects that meet the criteria of the request.
larsAttributes :: Lens' ListAttributesResponse [Attribute]
larsAttributes = lens _larsAttributes (\ s a -> s{_larsAttributes = a}) . _Default . _Coerce

-- | -- | The response status code.
larsResponseStatus :: Lens' ListAttributesResponse Int
larsResponseStatus = lens _larsResponseStatus (\ s a -> s{_larsResponseStatus = a})

instance NFData ListAttributesResponse where
