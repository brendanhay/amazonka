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
-- Module      : Network.AWS.EFS.ListTagsForResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all tags for a top-level EFS resource. You must provide the ID of the resource that you want to retrieve the tags for.
--
--
-- This operation requires permissions for the @elasticfilesystem:DescribeAccessPoints@ action.
module Network.AWS.EFS.ListTagsForResource
  ( -- * Creating a Request
    listTagsForResource,
    ListTagsForResource,

    -- * Request Lenses
    ltfrNextToken,
    ltfrMaxResults,
    ltfrResourceId,

    -- * Destructuring the Response
    listTagsForResourceResponse,
    ListTagsForResourceResponse,

    -- * Response Lenses
    ltfrrsNextToken,
    ltfrrsTags,
    ltfrrsResponseStatus,
  )
where

import Network.AWS.EFS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { _ltfrNextToken ::
      !(Maybe Text),
    _ltfrMaxResults :: !(Maybe Nat),
    _ltfrResourceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTagsForResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfrNextToken' - You can use @NextToken@ in a subsequent request to fetch the next page of access point descriptions if the response payload was paginated.
--
-- * 'ltfrMaxResults' - (Optional) Specifies the maximum number of tag objects to return in the response. The default value is 100.
--
-- * 'ltfrResourceId' - Specifies the EFS resource you want to retrieve tags for. You can retrieve tags for EFS file systems and access points using this API endpoint.
listTagsForResource ::
  -- | 'ltfrResourceId'
  Text ->
  ListTagsForResource
listTagsForResource pResourceId_ =
  ListTagsForResource'
    { _ltfrNextToken = Nothing,
      _ltfrMaxResults = Nothing,
      _ltfrResourceId = pResourceId_
    }

-- | You can use @NextToken@ in a subsequent request to fetch the next page of access point descriptions if the response payload was paginated.
ltfrNextToken :: Lens' ListTagsForResource (Maybe Text)
ltfrNextToken = lens _ltfrNextToken (\s a -> s {_ltfrNextToken = a})

-- | (Optional) Specifies the maximum number of tag objects to return in the response. The default value is 100.
ltfrMaxResults :: Lens' ListTagsForResource (Maybe Natural)
ltfrMaxResults = lens _ltfrMaxResults (\s a -> s {_ltfrMaxResults = a}) . mapping _Nat

-- | Specifies the EFS resource you want to retrieve tags for. You can retrieve tags for EFS file systems and access points using this API endpoint.
ltfrResourceId :: Lens' ListTagsForResource Text
ltfrResourceId = lens _ltfrResourceId (\s a -> s {_ltfrResourceId = a})

instance AWSRequest ListTagsForResource where
  type Rs ListTagsForResource = ListTagsForResourceResponse
  request = get efs
  response =
    receiveJSON
      ( \s h x ->
          ListTagsForResourceResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "Tags" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListTagsForResource

instance NFData ListTagsForResource

instance ToHeaders ListTagsForResource where
  toHeaders = const mempty

instance ToPath ListTagsForResource where
  toPath ListTagsForResource' {..} =
    mconcat ["/2015-02-01/resource-tags/", toBS _ltfrResourceId]

instance ToQuery ListTagsForResource where
  toQuery ListTagsForResource' {..} =
    mconcat
      ["NextToken" =: _ltfrNextToken, "MaxResults" =: _ltfrMaxResults]

-- | /See:/ 'listTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { _ltfrrsNextToken ::
      !(Maybe Text),
    _ltfrrsTags :: !(Maybe [Tag]),
    _ltfrrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTagsForResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfrrsNextToken' - @NextToken@ is present if the response payload is paginated. You can use @NextToken@ in a subsequent request to fetch the next page of access point descriptions.
--
-- * 'ltfrrsTags' - An array of the tags for the specified EFS resource.
--
-- * 'ltfrrsResponseStatus' - -- | The response status code.
listTagsForResourceResponse ::
  -- | 'ltfrrsResponseStatus'
  Int ->
  ListTagsForResourceResponse
listTagsForResourceResponse pResponseStatus_ =
  ListTagsForResourceResponse'
    { _ltfrrsNextToken = Nothing,
      _ltfrrsTags = Nothing,
      _ltfrrsResponseStatus = pResponseStatus_
    }

-- | @NextToken@ is present if the response payload is paginated. You can use @NextToken@ in a subsequent request to fetch the next page of access point descriptions.
ltfrrsNextToken :: Lens' ListTagsForResourceResponse (Maybe Text)
ltfrrsNextToken = lens _ltfrrsNextToken (\s a -> s {_ltfrrsNextToken = a})

-- | An array of the tags for the specified EFS resource.
ltfrrsTags :: Lens' ListTagsForResourceResponse [Tag]
ltfrrsTags = lens _ltfrrsTags (\s a -> s {_ltfrrsTags = a}) . _Default . _Coerce

-- | -- | The response status code.
ltfrrsResponseStatus :: Lens' ListTagsForResourceResponse Int
ltfrrsResponseStatus = lens _ltfrrsResponseStatus (\s a -> s {_ltfrrsResponseStatus = a})

instance NFData ListTagsForResourceResponse
