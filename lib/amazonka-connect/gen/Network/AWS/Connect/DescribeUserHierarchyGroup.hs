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
-- Module      : Network.AWS.Connect.DescribeUserHierarchyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified hierarchy group.
module Network.AWS.Connect.DescribeUserHierarchyGroup
  ( -- * Creating a Request
    describeUserHierarchyGroup,
    DescribeUserHierarchyGroup,

    -- * Request Lenses
    duhgHierarchyGroupId,
    duhgInstanceId,

    -- * Destructuring the Response
    describeUserHierarchyGroupResponse,
    DescribeUserHierarchyGroupResponse,

    -- * Response Lenses
    duhgrsHierarchyGroup,
    duhgrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeUserHierarchyGroup' smart constructor.
data DescribeUserHierarchyGroup = DescribeUserHierarchyGroup'
  { _duhgHierarchyGroupId ::
      !Text,
    _duhgInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeUserHierarchyGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duhgHierarchyGroupId' - The identifier of the hierarchy group.
--
-- * 'duhgInstanceId' - The identifier of the Amazon Connect instance.
describeUserHierarchyGroup ::
  -- | 'duhgHierarchyGroupId'
  Text ->
  -- | 'duhgInstanceId'
  Text ->
  DescribeUserHierarchyGroup
describeUserHierarchyGroup pHierarchyGroupId_ pInstanceId_ =
  DescribeUserHierarchyGroup'
    { _duhgHierarchyGroupId =
        pHierarchyGroupId_,
      _duhgInstanceId = pInstanceId_
    }

-- | The identifier of the hierarchy group.
duhgHierarchyGroupId :: Lens' DescribeUserHierarchyGroup Text
duhgHierarchyGroupId = lens _duhgHierarchyGroupId (\s a -> s {_duhgHierarchyGroupId = a})

-- | The identifier of the Amazon Connect instance.
duhgInstanceId :: Lens' DescribeUserHierarchyGroup Text
duhgInstanceId = lens _duhgInstanceId (\s a -> s {_duhgInstanceId = a})

instance AWSRequest DescribeUserHierarchyGroup where
  type
    Rs DescribeUserHierarchyGroup =
      DescribeUserHierarchyGroupResponse
  request = get connect
  response =
    receiveJSON
      ( \s h x ->
          DescribeUserHierarchyGroupResponse'
            <$> (x .?> "HierarchyGroup") <*> (pure (fromEnum s))
      )

instance Hashable DescribeUserHierarchyGroup

instance NFData DescribeUserHierarchyGroup

instance ToHeaders DescribeUserHierarchyGroup where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DescribeUserHierarchyGroup where
  toPath DescribeUserHierarchyGroup' {..} =
    mconcat
      [ "/user-hierarchy-groups/",
        toBS _duhgInstanceId,
        "/",
        toBS _duhgHierarchyGroupId
      ]

instance ToQuery DescribeUserHierarchyGroup where
  toQuery = const mempty

-- | /See:/ 'describeUserHierarchyGroupResponse' smart constructor.
data DescribeUserHierarchyGroupResponse = DescribeUserHierarchyGroupResponse'
  { _duhgrsHierarchyGroup ::
      !( Maybe
           HierarchyGroup
       ),
    _duhgrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeUserHierarchyGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duhgrsHierarchyGroup' - Information about the hierarchy group.
--
-- * 'duhgrsResponseStatus' - -- | The response status code.
describeUserHierarchyGroupResponse ::
  -- | 'duhgrsResponseStatus'
  Int ->
  DescribeUserHierarchyGroupResponse
describeUserHierarchyGroupResponse pResponseStatus_ =
  DescribeUserHierarchyGroupResponse'
    { _duhgrsHierarchyGroup =
        Nothing,
      _duhgrsResponseStatus = pResponseStatus_
    }

-- | Information about the hierarchy group.
duhgrsHierarchyGroup :: Lens' DescribeUserHierarchyGroupResponse (Maybe HierarchyGroup)
duhgrsHierarchyGroup = lens _duhgrsHierarchyGroup (\s a -> s {_duhgrsHierarchyGroup = a})

-- | -- | The response status code.
duhgrsResponseStatus :: Lens' DescribeUserHierarchyGroupResponse Int
duhgrsResponseStatus = lens _duhgrsResponseStatus (\s a -> s {_duhgrsResponseStatus = a})

instance NFData DescribeUserHierarchyGroupResponse
