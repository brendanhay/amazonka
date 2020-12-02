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
-- Module      : Network.AWS.CloudFront.ListKeyGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of key groups.
--
--
-- You can optionally specify the maximum number of items to receive in the response. If the total number of items in the list exceeds the maximum that you specify, or the default maximum, the response is paginated. To get the next page of items, send a subsequent request that specifies the @NextMarker@ value from the current response as the @Marker@ value in the subsequent request.
module Network.AWS.CloudFront.ListKeyGroups
  ( -- * Creating a Request
    listKeyGroups,
    ListKeyGroups,

    -- * Request Lenses
    lkgMarker,
    lkgMaxItems,

    -- * Destructuring the Response
    listKeyGroupsResponse,
    ListKeyGroupsResponse,

    -- * Response Lenses
    lkgrsKeyGroupList,
    lkgrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listKeyGroups' smart constructor.
data ListKeyGroups = ListKeyGroups'
  { _lkgMarker :: !(Maybe Text),
    _lkgMaxItems :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListKeyGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lkgMarker' - Use this field when paginating results to indicate where to begin in your list of key groups. The response includes key groups in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- * 'lkgMaxItems' - The maximum number of key groups that you want in the response.
listKeyGroups ::
  ListKeyGroups
listKeyGroups =
  ListKeyGroups' {_lkgMarker = Nothing, _lkgMaxItems = Nothing}

-- | Use this field when paginating results to indicate where to begin in your list of key groups. The response includes key groups in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
lkgMarker :: Lens' ListKeyGroups (Maybe Text)
lkgMarker = lens _lkgMarker (\s a -> s {_lkgMarker = a})

-- | The maximum number of key groups that you want in the response.
lkgMaxItems :: Lens' ListKeyGroups (Maybe Text)
lkgMaxItems = lens _lkgMaxItems (\s a -> s {_lkgMaxItems = a})

instance AWSRequest ListKeyGroups where
  type Rs ListKeyGroups = ListKeyGroupsResponse
  request = get cloudFront
  response =
    receiveXML
      ( \s h x ->
          ListKeyGroupsResponse' <$> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable ListKeyGroups

instance NFData ListKeyGroups

instance ToHeaders ListKeyGroups where
  toHeaders = const mempty

instance ToPath ListKeyGroups where
  toPath = const "/2020-05-31/key-group"

instance ToQuery ListKeyGroups where
  toQuery ListKeyGroups' {..} =
    mconcat ["Marker" =: _lkgMarker, "MaxItems" =: _lkgMaxItems]

-- | /See:/ 'listKeyGroupsResponse' smart constructor.
data ListKeyGroupsResponse = ListKeyGroupsResponse'
  { _lkgrsKeyGroupList ::
      !(Maybe KeyGroupList),
    _lkgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListKeyGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lkgrsKeyGroupList' - A list of key groups.
--
-- * 'lkgrsResponseStatus' - -- | The response status code.
listKeyGroupsResponse ::
  -- | 'lkgrsResponseStatus'
  Int ->
  ListKeyGroupsResponse
listKeyGroupsResponse pResponseStatus_ =
  ListKeyGroupsResponse'
    { _lkgrsKeyGroupList = Nothing,
      _lkgrsResponseStatus = pResponseStatus_
    }

-- | A list of key groups.
lkgrsKeyGroupList :: Lens' ListKeyGroupsResponse (Maybe KeyGroupList)
lkgrsKeyGroupList = lens _lkgrsKeyGroupList (\s a -> s {_lkgrsKeyGroupList = a})

-- | -- | The response status code.
lkgrsResponseStatus :: Lens' ListKeyGroupsResponse Int
lkgrsResponseStatus = lens _lkgrsResponseStatus (\s a -> s {_lkgrsResponseStatus = a})

instance NFData ListKeyGroupsResponse
