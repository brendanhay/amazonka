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
-- Module      : Network.AWS.SSM.DescribePatchGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all patch groups that have been registered with patch baselines.
--
--
module Network.AWS.SSM.DescribePatchGroups
    (
    -- * Creating a Request
      describePatchGroups
    , DescribePatchGroups
    -- * Request Lenses
    , dpgFilters
    , dpgNextToken
    , dpgMaxResults

    -- * Destructuring the Response
    , describePatchGroupsResponse
    , DescribePatchGroupsResponse
    -- * Response Lenses
    , dpgrsMappings
    , dpgrsNextToken
    , dpgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'describePatchGroups' smart constructor.
data DescribePatchGroups = DescribePatchGroups'
  { _dpgFilters    :: !(Maybe [PatchOrchestratorFilter])
  , _dpgNextToken  :: !(Maybe Text)
  , _dpgMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePatchGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpgFilters' - One or more filters. Use a filter to return a more specific list of results.
--
-- * 'dpgNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dpgMaxResults' - The maximum number of patch groups to return (per page).
describePatchGroups
    :: DescribePatchGroups
describePatchGroups =
  DescribePatchGroups'
    {_dpgFilters = Nothing, _dpgNextToken = Nothing, _dpgMaxResults = Nothing}


-- | One or more filters. Use a filter to return a more specific list of results.
dpgFilters :: Lens' DescribePatchGroups [PatchOrchestratorFilter]
dpgFilters = lens _dpgFilters (\ s a -> s{_dpgFilters = a}) . _Default . _Coerce

-- | The token for the next set of items to return. (You received this token from a previous call.)
dpgNextToken :: Lens' DescribePatchGroups (Maybe Text)
dpgNextToken = lens _dpgNextToken (\ s a -> s{_dpgNextToken = a})

-- | The maximum number of patch groups to return (per page).
dpgMaxResults :: Lens' DescribePatchGroups (Maybe Natural)
dpgMaxResults = lens _dpgMaxResults (\ s a -> s{_dpgMaxResults = a}) . mapping _Nat

instance AWSRequest DescribePatchGroups where
        type Rs DescribePatchGroups =
             DescribePatchGroupsResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribePatchGroupsResponse' <$>
                   (x .?> "Mappings" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribePatchGroups where

instance NFData DescribePatchGroups where

instance ToHeaders DescribePatchGroups where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribePatchGroups" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribePatchGroups where
        toJSON DescribePatchGroups'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _dpgFilters,
                  ("NextToken" .=) <$> _dpgNextToken,
                  ("MaxResults" .=) <$> _dpgMaxResults])

instance ToPath DescribePatchGroups where
        toPath = const "/"

instance ToQuery DescribePatchGroups where
        toQuery = const mempty

-- | /See:/ 'describePatchGroupsResponse' smart constructor.
data DescribePatchGroupsResponse = DescribePatchGroupsResponse'
  { _dpgrsMappings       :: !(Maybe [PatchGroupPatchBaselineMapping])
  , _dpgrsNextToken      :: !(Maybe Text)
  , _dpgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePatchGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpgrsMappings' - Each entry in the array contains: PatchGroup: string (between 1 and 256 characters, Regex: ^([\p{L}\p{Z}\p{N}_.:/=+\-@]*)$) PatchBaselineIdentity: A PatchBaselineIdentity element.
--
-- * 'dpgrsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'dpgrsResponseStatus' - -- | The response status code.
describePatchGroupsResponse
    :: Int -- ^ 'dpgrsResponseStatus'
    -> DescribePatchGroupsResponse
describePatchGroupsResponse pResponseStatus_ =
  DescribePatchGroupsResponse'
    { _dpgrsMappings = Nothing
    , _dpgrsNextToken = Nothing
    , _dpgrsResponseStatus = pResponseStatus_
    }


-- | Each entry in the array contains: PatchGroup: string (between 1 and 256 characters, Regex: ^([\p{L}\p{Z}\p{N}_.:/=+\-@]*)$) PatchBaselineIdentity: A PatchBaselineIdentity element.
dpgrsMappings :: Lens' DescribePatchGroupsResponse [PatchGroupPatchBaselineMapping]
dpgrsMappings = lens _dpgrsMappings (\ s a -> s{_dpgrsMappings = a}) . _Default . _Coerce

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
dpgrsNextToken :: Lens' DescribePatchGroupsResponse (Maybe Text)
dpgrsNextToken = lens _dpgrsNextToken (\ s a -> s{_dpgrsNextToken = a})

-- | -- | The response status code.
dpgrsResponseStatus :: Lens' DescribePatchGroupsResponse Int
dpgrsResponseStatus = lens _dpgrsResponseStatus (\ s a -> s{_dpgrsResponseStatus = a})

instance NFData DescribePatchGroupsResponse where
