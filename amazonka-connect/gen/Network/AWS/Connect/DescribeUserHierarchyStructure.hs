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
-- Module      : Network.AWS.Connect.DescribeUserHierarchyStructure
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a @HiearchyGroupStructure@ object, which contains data about the levels in the agent hierarchy.
--
--
module Network.AWS.Connect.DescribeUserHierarchyStructure
    (
    -- * Creating a Request
      describeUserHierarchyStructure
    , DescribeUserHierarchyStructure
    -- * Request Lenses
    , duhsInstanceId

    -- * Destructuring the Response
    , describeUserHierarchyStructureResponse
    , DescribeUserHierarchyStructureResponse
    -- * Response Lenses
    , duhsrsHierarchyStructure
    , duhsrsResponseStatus
    ) where

import Network.AWS.Connect.Types
import Network.AWS.Connect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeUserHierarchyStructure' smart constructor.
newtype DescribeUserHierarchyStructure = DescribeUserHierarchyStructure'
  { _duhsInstanceId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeUserHierarchyStructure' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duhsInstanceId' - The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
describeUserHierarchyStructure
    :: Text -- ^ 'duhsInstanceId'
    -> DescribeUserHierarchyStructure
describeUserHierarchyStructure pInstanceId_ =
  DescribeUserHierarchyStructure' {_duhsInstanceId = pInstanceId_}


-- | The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
duhsInstanceId :: Lens' DescribeUserHierarchyStructure Text
duhsInstanceId = lens _duhsInstanceId (\ s a -> s{_duhsInstanceId = a})

instance AWSRequest DescribeUserHierarchyStructure
         where
        type Rs DescribeUserHierarchyStructure =
             DescribeUserHierarchyStructureResponse
        request = get connect
        response
          = receiveJSON
              (\ s h x ->
                 DescribeUserHierarchyStructureResponse' <$>
                   (x .?> "HierarchyStructure") <*> (pure (fromEnum s)))

instance Hashable DescribeUserHierarchyStructure
         where

instance NFData DescribeUserHierarchyStructure where

instance ToHeaders DescribeUserHierarchyStructure
         where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeUserHierarchyStructure where
        toPath DescribeUserHierarchyStructure'{..}
          = mconcat
              ["/user-hierarchy-structure/", toBS _duhsInstanceId]

instance ToQuery DescribeUserHierarchyStructure where
        toQuery = const mempty

-- | /See:/ 'describeUserHierarchyStructureResponse' smart constructor.
data DescribeUserHierarchyStructureResponse = DescribeUserHierarchyStructureResponse'
  { _duhsrsHierarchyStructure :: !(Maybe HierarchyStructure)
  , _duhsrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeUserHierarchyStructureResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'duhsrsHierarchyStructure' - A @HierarchyStructure@ object.
--
-- * 'duhsrsResponseStatus' - -- | The response status code.
describeUserHierarchyStructureResponse
    :: Int -- ^ 'duhsrsResponseStatus'
    -> DescribeUserHierarchyStructureResponse
describeUserHierarchyStructureResponse pResponseStatus_ =
  DescribeUserHierarchyStructureResponse'
    { _duhsrsHierarchyStructure = Nothing
    , _duhsrsResponseStatus = pResponseStatus_
    }


-- | A @HierarchyStructure@ object.
duhsrsHierarchyStructure :: Lens' DescribeUserHierarchyStructureResponse (Maybe HierarchyStructure)
duhsrsHierarchyStructure = lens _duhsrsHierarchyStructure (\ s a -> s{_duhsrsHierarchyStructure = a})

-- | -- | The response status code.
duhsrsResponseStatus :: Lens' DescribeUserHierarchyStructureResponse Int
duhsrsResponseStatus = lens _duhsrsResponseStatus (\ s a -> s{_duhsrsResponseStatus = a})

instance NFData
           DescribeUserHierarchyStructureResponse
         where
