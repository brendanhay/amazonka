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
-- Module      : Network.AWS.Connect.DescribeUserHierarchyGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a @HierarchyGroup@ object that includes information about a hierarchy group in your instance.
--
--
module Network.AWS.Connect.DescribeUserHierarchyGroup
    (
    -- * Creating a Request
      describeUserHierarchyGroup
    , DescribeUserHierarchyGroup
    -- * Request Lenses
    , duhgHierarchyGroupId
    , duhgInstanceId

    -- * Destructuring the Response
    , describeUserHierarchyGroupResponse
    , DescribeUserHierarchyGroupResponse
    -- * Response Lenses
    , duhgrsHierarchyGroup
    , duhgrsResponseStatus
    ) where

import Network.AWS.Connect.Types
import Network.AWS.Connect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeUserHierarchyGroup' smart constructor.
data DescribeUserHierarchyGroup = DescribeUserHierarchyGroup'
  { _duhgHierarchyGroupId :: !Text
  , _duhgInstanceId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeUserHierarchyGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duhgHierarchyGroupId' - The identifier for the hierarchy group to return.
--
-- * 'duhgInstanceId' - The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
describeUserHierarchyGroup
    :: Text -- ^ 'duhgHierarchyGroupId'
    -> Text -- ^ 'duhgInstanceId'
    -> DescribeUserHierarchyGroup
describeUserHierarchyGroup pHierarchyGroupId_ pInstanceId_ =
  DescribeUserHierarchyGroup'
    {_duhgHierarchyGroupId = pHierarchyGroupId_, _duhgInstanceId = pInstanceId_}


-- | The identifier for the hierarchy group to return.
duhgHierarchyGroupId :: Lens' DescribeUserHierarchyGroup Text
duhgHierarchyGroupId = lens _duhgHierarchyGroupId (\ s a -> s{_duhgHierarchyGroupId = a})

-- | The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
duhgInstanceId :: Lens' DescribeUserHierarchyGroup Text
duhgInstanceId = lens _duhgInstanceId (\ s a -> s{_duhgInstanceId = a})

instance AWSRequest DescribeUserHierarchyGroup where
        type Rs DescribeUserHierarchyGroup =
             DescribeUserHierarchyGroupResponse
        request = get connect
        response
          = receiveJSON
              (\ s h x ->
                 DescribeUserHierarchyGroupResponse' <$>
                   (x .?> "HierarchyGroup") <*> (pure (fromEnum s)))

instance Hashable DescribeUserHierarchyGroup where

instance NFData DescribeUserHierarchyGroup where

instance ToHeaders DescribeUserHierarchyGroup where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeUserHierarchyGroup where
        toPath DescribeUserHierarchyGroup'{..}
          = mconcat
              ["/user-hierarchy-groups/", toBS _duhgInstanceId,
               "/", toBS _duhgHierarchyGroupId]

instance ToQuery DescribeUserHierarchyGroup where
        toQuery = const mempty

-- | /See:/ 'describeUserHierarchyGroupResponse' smart constructor.
data DescribeUserHierarchyGroupResponse = DescribeUserHierarchyGroupResponse'
  { _duhgrsHierarchyGroup :: !(Maybe HierarchyGroup)
  , _duhgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeUserHierarchyGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duhgrsHierarchyGroup' - Returns a @HierarchyGroup@ object.
--
-- * 'duhgrsResponseStatus' - -- | The response status code.
describeUserHierarchyGroupResponse
    :: Int -- ^ 'duhgrsResponseStatus'
    -> DescribeUserHierarchyGroupResponse
describeUserHierarchyGroupResponse pResponseStatus_ =
  DescribeUserHierarchyGroupResponse'
    {_duhgrsHierarchyGroup = Nothing, _duhgrsResponseStatus = pResponseStatus_}


-- | Returns a @HierarchyGroup@ object.
duhgrsHierarchyGroup :: Lens' DescribeUserHierarchyGroupResponse (Maybe HierarchyGroup)
duhgrsHierarchyGroup = lens _duhgrsHierarchyGroup (\ s a -> s{_duhgrsHierarchyGroup = a})

-- | -- | The response status code.
duhgrsResponseStatus :: Lens' DescribeUserHierarchyGroupResponse Int
duhgrsResponseStatus = lens _duhgrsResponseStatus (\ s a -> s{_duhgrsResponseStatus = a})

instance NFData DescribeUserHierarchyGroupResponse
         where
