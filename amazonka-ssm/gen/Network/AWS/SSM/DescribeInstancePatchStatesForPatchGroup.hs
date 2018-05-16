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
-- Module      : Network.AWS.SSM.DescribeInstancePatchStatesForPatchGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the high-level patch state for the instances in the specified patch group.
--
--
module Network.AWS.SSM.DescribeInstancePatchStatesForPatchGroup
    (
    -- * Creating a Request
      describeInstancePatchStatesForPatchGroup
    , DescribeInstancePatchStatesForPatchGroup
    -- * Request Lenses
    , dipsfpgFilters
    , dipsfpgNextToken
    , dipsfpgMaxResults
    , dipsfpgPatchGroup

    -- * Destructuring the Response
    , describeInstancePatchStatesForPatchGroupResponse
    , DescribeInstancePatchStatesForPatchGroupResponse
    -- * Response Lenses
    , dipsfpgrsNextToken
    , dipsfpgrsInstancePatchStates
    , dipsfpgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'describeInstancePatchStatesForPatchGroup' smart constructor.
data DescribeInstancePatchStatesForPatchGroup = DescribeInstancePatchStatesForPatchGroup'
  { _dipsfpgFilters    :: !(Maybe [InstancePatchStateFilter])
  , _dipsfpgNextToken  :: !(Maybe Text)
  , _dipsfpgMaxResults :: !(Maybe Nat)
  , _dipsfpgPatchGroup :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInstancePatchStatesForPatchGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dipsfpgFilters' - Each entry in the array is a structure containing: Key (string between 1 and 200 characters) Values (array containing a single string) Type (string "Equal", "NotEqual", "LessThan", "GreaterThan")
--
-- * 'dipsfpgNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dipsfpgMaxResults' - The maximum number of patches to return (per page).
--
-- * 'dipsfpgPatchGroup' - The name of the patch group for which the patch state information should be retrieved.
describeInstancePatchStatesForPatchGroup
    :: Text -- ^ 'dipsfpgPatchGroup'
    -> DescribeInstancePatchStatesForPatchGroup
describeInstancePatchStatesForPatchGroup pPatchGroup_ =
  DescribeInstancePatchStatesForPatchGroup'
    { _dipsfpgFilters = Nothing
    , _dipsfpgNextToken = Nothing
    , _dipsfpgMaxResults = Nothing
    , _dipsfpgPatchGroup = pPatchGroup_
    }


-- | Each entry in the array is a structure containing: Key (string between 1 and 200 characters) Values (array containing a single string) Type (string "Equal", "NotEqual", "LessThan", "GreaterThan")
dipsfpgFilters :: Lens' DescribeInstancePatchStatesForPatchGroup [InstancePatchStateFilter]
dipsfpgFilters = lens _dipsfpgFilters (\ s a -> s{_dipsfpgFilters = a}) . _Default . _Coerce

-- | The token for the next set of items to return. (You received this token from a previous call.)
dipsfpgNextToken :: Lens' DescribeInstancePatchStatesForPatchGroup (Maybe Text)
dipsfpgNextToken = lens _dipsfpgNextToken (\ s a -> s{_dipsfpgNextToken = a})

-- | The maximum number of patches to return (per page).
dipsfpgMaxResults :: Lens' DescribeInstancePatchStatesForPatchGroup (Maybe Natural)
dipsfpgMaxResults = lens _dipsfpgMaxResults (\ s a -> s{_dipsfpgMaxResults = a}) . mapping _Nat

-- | The name of the patch group for which the patch state information should be retrieved.
dipsfpgPatchGroup :: Lens' DescribeInstancePatchStatesForPatchGroup Text
dipsfpgPatchGroup = lens _dipsfpgPatchGroup (\ s a -> s{_dipsfpgPatchGroup = a})

instance AWSRequest
           DescribeInstancePatchStatesForPatchGroup
         where
        type Rs DescribeInstancePatchStatesForPatchGroup =
             DescribeInstancePatchStatesForPatchGroupResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribeInstancePatchStatesForPatchGroupResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "InstancePatchStates")
                     <*> (pure (fromEnum s)))

instance Hashable
           DescribeInstancePatchStatesForPatchGroup
         where

instance NFData
           DescribeInstancePatchStatesForPatchGroup
         where

instance ToHeaders
           DescribeInstancePatchStatesForPatchGroup
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribeInstancePatchStatesForPatchGroup"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON
           DescribeInstancePatchStatesForPatchGroup
         where
        toJSON DescribeInstancePatchStatesForPatchGroup'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _dipsfpgFilters,
                  ("NextToken" .=) <$> _dipsfpgNextToken,
                  ("MaxResults" .=) <$> _dipsfpgMaxResults,
                  Just ("PatchGroup" .= _dipsfpgPatchGroup)])

instance ToPath
           DescribeInstancePatchStatesForPatchGroup
         where
        toPath = const "/"

instance ToQuery
           DescribeInstancePatchStatesForPatchGroup
         where
        toQuery = const mempty

-- | /See:/ 'describeInstancePatchStatesForPatchGroupResponse' smart constructor.
data DescribeInstancePatchStatesForPatchGroupResponse = DescribeInstancePatchStatesForPatchGroupResponse'
  { _dipsfpgrsNextToken           :: !(Maybe Text)
  , _dipsfpgrsInstancePatchStates :: !(Maybe (List1 InstancePatchState))
  , _dipsfpgrsResponseStatus      :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInstancePatchStatesForPatchGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dipsfpgrsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'dipsfpgrsInstancePatchStates' - The high-level patch state for the requested instances.
--
-- * 'dipsfpgrsResponseStatus' - -- | The response status code.
describeInstancePatchStatesForPatchGroupResponse
    :: Int -- ^ 'dipsfpgrsResponseStatus'
    -> DescribeInstancePatchStatesForPatchGroupResponse
describeInstancePatchStatesForPatchGroupResponse pResponseStatus_ =
  DescribeInstancePatchStatesForPatchGroupResponse'
    { _dipsfpgrsNextToken = Nothing
    , _dipsfpgrsInstancePatchStates = Nothing
    , _dipsfpgrsResponseStatus = pResponseStatus_
    }


-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
dipsfpgrsNextToken :: Lens' DescribeInstancePatchStatesForPatchGroupResponse (Maybe Text)
dipsfpgrsNextToken = lens _dipsfpgrsNextToken (\ s a -> s{_dipsfpgrsNextToken = a})

-- | The high-level patch state for the requested instances.
dipsfpgrsInstancePatchStates :: Lens' DescribeInstancePatchStatesForPatchGroupResponse (Maybe (NonEmpty InstancePatchState))
dipsfpgrsInstancePatchStates = lens _dipsfpgrsInstancePatchStates (\ s a -> s{_dipsfpgrsInstancePatchStates = a}) . mapping _List1

-- | -- | The response status code.
dipsfpgrsResponseStatus :: Lens' DescribeInstancePatchStatesForPatchGroupResponse Int
dipsfpgrsResponseStatus = lens _dipsfpgrsResponseStatus (\ s a -> s{_dipsfpgrsResponseStatus = a})

instance NFData
           DescribeInstancePatchStatesForPatchGroupResponse
         where
