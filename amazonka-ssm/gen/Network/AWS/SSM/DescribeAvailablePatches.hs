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
-- Module      : Network.AWS.SSM.DescribeAvailablePatches
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all patches that could possibly be included in a patch baseline.
--
--
module Network.AWS.SSM.DescribeAvailablePatches
    (
    -- * Creating a Request
      describeAvailablePatches
    , DescribeAvailablePatches
    -- * Request Lenses
    , dapFilters
    , dapNextToken
    , dapMaxResults

    -- * Destructuring the Response
    , describeAvailablePatchesResponse
    , DescribeAvailablePatchesResponse
    -- * Response Lenses
    , daprsPatches
    , daprsNextToken
    , daprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'describeAvailablePatches' smart constructor.
data DescribeAvailablePatches = DescribeAvailablePatches'
  { _dapFilters    :: !(Maybe [PatchOrchestratorFilter])
  , _dapNextToken  :: !(Maybe Text)
  , _dapMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAvailablePatches' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dapFilters' - Filters used to scope down the returned patches.
--
-- * 'dapNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dapMaxResults' - The maximum number of patches to return (per page).
describeAvailablePatches
    :: DescribeAvailablePatches
describeAvailablePatches =
  DescribeAvailablePatches'
    {_dapFilters = Nothing, _dapNextToken = Nothing, _dapMaxResults = Nothing}


-- | Filters used to scope down the returned patches.
dapFilters :: Lens' DescribeAvailablePatches [PatchOrchestratorFilter]
dapFilters = lens _dapFilters (\ s a -> s{_dapFilters = a}) . _Default . _Coerce

-- | The token for the next set of items to return. (You received this token from a previous call.)
dapNextToken :: Lens' DescribeAvailablePatches (Maybe Text)
dapNextToken = lens _dapNextToken (\ s a -> s{_dapNextToken = a})

-- | The maximum number of patches to return (per page).
dapMaxResults :: Lens' DescribeAvailablePatches (Maybe Natural)
dapMaxResults = lens _dapMaxResults (\ s a -> s{_dapMaxResults = a}) . mapping _Nat

instance AWSRequest DescribeAvailablePatches where
        type Rs DescribeAvailablePatches =
             DescribeAvailablePatchesResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAvailablePatchesResponse' <$>
                   (x .?> "Patches" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeAvailablePatches where

instance NFData DescribeAvailablePatches where

instance ToHeaders DescribeAvailablePatches where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribeAvailablePatches" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeAvailablePatches where
        toJSON DescribeAvailablePatches'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _dapFilters,
                  ("NextToken" .=) <$> _dapNextToken,
                  ("MaxResults" .=) <$> _dapMaxResults])

instance ToPath DescribeAvailablePatches where
        toPath = const "/"

instance ToQuery DescribeAvailablePatches where
        toQuery = const mempty

-- | /See:/ 'describeAvailablePatchesResponse' smart constructor.
data DescribeAvailablePatchesResponse = DescribeAvailablePatchesResponse'
  { _daprsPatches        :: !(Maybe [Patch])
  , _daprsNextToken      :: !(Maybe Text)
  , _daprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAvailablePatchesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daprsPatches' - An array of patches. Each entry in the array is a patch structure.
--
-- * 'daprsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'daprsResponseStatus' - -- | The response status code.
describeAvailablePatchesResponse
    :: Int -- ^ 'daprsResponseStatus'
    -> DescribeAvailablePatchesResponse
describeAvailablePatchesResponse pResponseStatus_ =
  DescribeAvailablePatchesResponse'
    { _daprsPatches = Nothing
    , _daprsNextToken = Nothing
    , _daprsResponseStatus = pResponseStatus_
    }


-- | An array of patches. Each entry in the array is a patch structure.
daprsPatches :: Lens' DescribeAvailablePatchesResponse [Patch]
daprsPatches = lens _daprsPatches (\ s a -> s{_daprsPatches = a}) . _Default . _Coerce

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
daprsNextToken :: Lens' DescribeAvailablePatchesResponse (Maybe Text)
daprsNextToken = lens _daprsNextToken (\ s a -> s{_daprsNextToken = a})

-- | -- | The response status code.
daprsResponseStatus :: Lens' DescribeAvailablePatchesResponse Int
daprsResponseStatus = lens _daprsResponseStatus (\ s a -> s{_daprsResponseStatus = a})

instance NFData DescribeAvailablePatchesResponse
         where
