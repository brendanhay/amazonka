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
-- Module      : Network.AWS.CloudFront.ListDistributionsByKeyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of distribution IDs for distributions that have a cache behavior that references the specified key group.
--
--
-- You can optionally specify the maximum number of items to receive in the response. If the total number of items in the list exceeds the maximum that you specify, or the default maximum, the response is paginated. To get the next page of items, send a subsequent request that specifies the @NextMarker@ value from the current response as the @Marker@ value in the subsequent request.
module Network.AWS.CloudFront.ListDistributionsByKeyGroup
  ( -- * Creating a Request
    listDistributionsByKeyGroup,
    ListDistributionsByKeyGroup,

    -- * Request Lenses
    ldbkgMarker,
    ldbkgMaxItems,
    ldbkgKeyGroupId,

    -- * Destructuring the Response
    listDistributionsByKeyGroupResponse,
    ListDistributionsByKeyGroupResponse,

    -- * Response Lenses
    ldbkgrsDistributionIdList,
    ldbkgrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listDistributionsByKeyGroup' smart constructor.
data ListDistributionsByKeyGroup = ListDistributionsByKeyGroup'
  { _ldbkgMarker ::
      !(Maybe Text),
    _ldbkgMaxItems :: !(Maybe Text),
    _ldbkgKeyGroupId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDistributionsByKeyGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldbkgMarker' - Use this field when paginating results to indicate where to begin in your list of distribution IDs. The response includes distribution IDs in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- * 'ldbkgMaxItems' - The maximum number of distribution IDs that you want in the response.
--
-- * 'ldbkgKeyGroupId' - The ID of the key group whose associated distribution IDs you are listing.
listDistributionsByKeyGroup ::
  -- | 'ldbkgKeyGroupId'
  Text ->
  ListDistributionsByKeyGroup
listDistributionsByKeyGroup pKeyGroupId_ =
  ListDistributionsByKeyGroup'
    { _ldbkgMarker = Nothing,
      _ldbkgMaxItems = Nothing,
      _ldbkgKeyGroupId = pKeyGroupId_
    }

-- | Use this field when paginating results to indicate where to begin in your list of distribution IDs. The response includes distribution IDs in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
ldbkgMarker :: Lens' ListDistributionsByKeyGroup (Maybe Text)
ldbkgMarker = lens _ldbkgMarker (\s a -> s {_ldbkgMarker = a})

-- | The maximum number of distribution IDs that you want in the response.
ldbkgMaxItems :: Lens' ListDistributionsByKeyGroup (Maybe Text)
ldbkgMaxItems = lens _ldbkgMaxItems (\s a -> s {_ldbkgMaxItems = a})

-- | The ID of the key group whose associated distribution IDs you are listing.
ldbkgKeyGroupId :: Lens' ListDistributionsByKeyGroup Text
ldbkgKeyGroupId = lens _ldbkgKeyGroupId (\s a -> s {_ldbkgKeyGroupId = a})

instance AWSRequest ListDistributionsByKeyGroup where
  type
    Rs ListDistributionsByKeyGroup =
      ListDistributionsByKeyGroupResponse
  request = get cloudFront
  response =
    receiveXML
      ( \s h x ->
          ListDistributionsByKeyGroupResponse'
            <$> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable ListDistributionsByKeyGroup

instance NFData ListDistributionsByKeyGroup

instance ToHeaders ListDistributionsByKeyGroup where
  toHeaders = const mempty

instance ToPath ListDistributionsByKeyGroup where
  toPath ListDistributionsByKeyGroup' {..} =
    mconcat
      ["/2020-05-31/distributionsByKeyGroupId/", toBS _ldbkgKeyGroupId]

instance ToQuery ListDistributionsByKeyGroup where
  toQuery ListDistributionsByKeyGroup' {..} =
    mconcat ["Marker" =: _ldbkgMarker, "MaxItems" =: _ldbkgMaxItems]

-- | /See:/ 'listDistributionsByKeyGroupResponse' smart constructor.
data ListDistributionsByKeyGroupResponse = ListDistributionsByKeyGroupResponse'
  { _ldbkgrsDistributionIdList ::
      !( Maybe
           DistributionIdList
       ),
    _ldbkgrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListDistributionsByKeyGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldbkgrsDistributionIdList' - Undocumented member.
--
-- * 'ldbkgrsResponseStatus' - -- | The response status code.
listDistributionsByKeyGroupResponse ::
  -- | 'ldbkgrsResponseStatus'
  Int ->
  ListDistributionsByKeyGroupResponse
listDistributionsByKeyGroupResponse pResponseStatus_ =
  ListDistributionsByKeyGroupResponse'
    { _ldbkgrsDistributionIdList =
        Nothing,
      _ldbkgrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
ldbkgrsDistributionIdList :: Lens' ListDistributionsByKeyGroupResponse (Maybe DistributionIdList)
ldbkgrsDistributionIdList = lens _ldbkgrsDistributionIdList (\s a -> s {_ldbkgrsDistributionIdList = a})

-- | -- | The response status code.
ldbkgrsResponseStatus :: Lens' ListDistributionsByKeyGroupResponse Int
ldbkgrsResponseStatus = lens _ldbkgrsResponseStatus (\s a -> s {_ldbkgrsResponseStatus = a})

instance NFData ListDistributionsByKeyGroupResponse
