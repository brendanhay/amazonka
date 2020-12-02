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
-- Module      : Network.AWS.ResourceGroups.ListGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of existing resource groups in your account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ResourceGroups.ListGroups
  ( -- * Creating a Request
    listGroups,
    ListGroups,

    -- * Request Lenses
    lgFilters,
    lgNextToken,
    lgMaxResults,

    -- * Destructuring the Response
    listGroupsResponse,
    ListGroupsResponse,

    -- * Response Lenses
    lgrsGroups,
    lgrsNextToken,
    lgrsGroupIdentifiers,
    lgrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.ResourceGroups.Types
import Network.AWS.Response

-- | /See:/ 'listGroups' smart constructor.
data ListGroups = ListGroups'
  { _lgFilters :: !(Maybe [GroupFilter]),
    _lgNextToken :: !(Maybe Text),
    _lgMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgFilters' - Filters, formatted as 'GroupFilter' objects, that you want to apply to a @ListGroups@ operation.     * @resource-type@ - Filter the results to include only those of the specified resource types. Specify up to five resource types in the format @AWS::/ServiceCode/ ::/ResourceType/ @ . For example, @AWS::EC2::Instance@ , or @AWS::S3::Bucket@ .     * @configuration-type@ - Filter the results to include only those groups that have the specified configuration types attached. The current supported values are:     * AWS:EC2::CapacityReservationPool
--
-- * 'lgNextToken' - The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value provided by a previous call's @NextToken@ response to indicate where the output should continue from.
--
-- * 'lgMaxResults' - The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that the service might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
listGroups ::
  ListGroups
listGroups =
  ListGroups'
    { _lgFilters = Nothing,
      _lgNextToken = Nothing,
      _lgMaxResults = Nothing
    }

-- | Filters, formatted as 'GroupFilter' objects, that you want to apply to a @ListGroups@ operation.     * @resource-type@ - Filter the results to include only those of the specified resource types. Specify up to five resource types in the format @AWS::/ServiceCode/ ::/ResourceType/ @ . For example, @AWS::EC2::Instance@ , or @AWS::S3::Bucket@ .     * @configuration-type@ - Filter the results to include only those groups that have the specified configuration types attached. The current supported values are:     * AWS:EC2::CapacityReservationPool
lgFilters :: Lens' ListGroups [GroupFilter]
lgFilters = lens _lgFilters (\s a -> s {_lgFilters = a}) . _Default . _Coerce

-- | The parameter for receiving additional results if you receive a @NextToken@ response in a previous request. A @NextToken@ response indicates that more output is available. Set this parameter to the value provided by a previous call's @NextToken@ response to indicate where the output should continue from.
lgNextToken :: Lens' ListGroups (Maybe Text)
lgNextToken = lens _lgNextToken (\s a -> s {_lgNextToken = a})

-- | The total number of results that you want included on each page of the response. If you do not include this parameter, it defaults to a value that is specific to the operation. If additional items exist beyond the maximum you specify, the @NextToken@ response element is present and has a value (is not null). Include that value as the @NextToken@ request parameter in the next call to the operation to get the next part of the results. Note that the service might return fewer results than the maximum even when there are more results available. You should check @NextToken@ after every operation to ensure that you receive all of the results.
lgMaxResults :: Lens' ListGroups (Maybe Natural)
lgMaxResults = lens _lgMaxResults (\s a -> s {_lgMaxResults = a}) . mapping _Nat

instance AWSPager ListGroups where
  page rq rs
    | stop (rs ^. lgrsNextToken) = Nothing
    | stop (rs ^. lgrsGroupIdentifiers) = Nothing
    | stop (rs ^. lgrsGroups) = Nothing
    | otherwise = Just $ rq & lgNextToken .~ rs ^. lgrsNextToken

instance AWSRequest ListGroups where
  type Rs ListGroups = ListGroupsResponse
  request = postJSON resourceGroups
  response =
    receiveJSON
      ( \s h x ->
          ListGroupsResponse'
            <$> (x .?> "Groups" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (x .?> "GroupIdentifiers" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListGroups

instance NFData ListGroups

instance ToHeaders ListGroups where
  toHeaders = const mempty

instance ToJSON ListGroups where
  toJSON ListGroups' {..} =
    object (catMaybes [("Filters" .=) <$> _lgFilters])

instance ToPath ListGroups where
  toPath = const "/groups-list"

instance ToQuery ListGroups where
  toQuery ListGroups' {..} =
    mconcat
      ["nextToken" =: _lgNextToken, "maxResults" =: _lgMaxResults]

-- | /See:/ 'listGroupsResponse' smart constructor.
data ListGroupsResponse = ListGroupsResponse'
  { _lgrsGroups ::
      !(Maybe [Group]),
    _lgrsNextToken :: !(Maybe Text),
    _lgrsGroupIdentifiers :: !(Maybe [GroupIdentifier]),
    _lgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgrsGroups' - This output element is deprecated and shouldn't be used. Refer to @GroupIdentifiers@ instead.
--
-- * 'lgrsNextToken' - If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
--
-- * 'lgrsGroupIdentifiers' - A list of 'GroupIdentifier' objects. Each identifier is an object that contains both the @Name@ and the @GroupArn@ .
--
-- * 'lgrsResponseStatus' - -- | The response status code.
listGroupsResponse ::
  -- | 'lgrsResponseStatus'
  Int ->
  ListGroupsResponse
listGroupsResponse pResponseStatus_ =
  ListGroupsResponse'
    { _lgrsGroups = Nothing,
      _lgrsNextToken = Nothing,
      _lgrsGroupIdentifiers = Nothing,
      _lgrsResponseStatus = pResponseStatus_
    }

-- | This output element is deprecated and shouldn't be used. Refer to @GroupIdentifiers@ instead.
lgrsGroups :: Lens' ListGroupsResponse [Group]
lgrsGroups = lens _lgrsGroups (\s a -> s {_lgrsGroups = a}) . _Default . _Coerce

-- | If present, indicates that more output is available than is included in the current response. Use this value in the @NextToken@ request parameter in a subsequent call to the operation to get the next part of the output. You should repeat this until the @NextToken@ response element comes back as @null@ .
lgrsNextToken :: Lens' ListGroupsResponse (Maybe Text)
lgrsNextToken = lens _lgrsNextToken (\s a -> s {_lgrsNextToken = a})

-- | A list of 'GroupIdentifier' objects. Each identifier is an object that contains both the @Name@ and the @GroupArn@ .
lgrsGroupIdentifiers :: Lens' ListGroupsResponse [GroupIdentifier]
lgrsGroupIdentifiers = lens _lgrsGroupIdentifiers (\s a -> s {_lgrsGroupIdentifiers = a}) . _Default . _Coerce

-- | -- | The response status code.
lgrsResponseStatus :: Lens' ListGroupsResponse Int
lgrsResponseStatus = lens _lgrsResponseStatus (\s a -> s {_lgrsResponseStatus = a})

instance NFData ListGroupsResponse
