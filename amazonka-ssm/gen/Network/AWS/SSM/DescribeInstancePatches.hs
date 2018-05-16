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
-- Module      : Network.AWS.SSM.DescribeInstancePatches
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the patches on the specified instance and their state relative to the patch baseline being used for the instance.
--
--
module Network.AWS.SSM.DescribeInstancePatches
    (
    -- * Creating a Request
      describeInstancePatches
    , DescribeInstancePatches
    -- * Request Lenses
    , dipFilters
    , dipNextToken
    , dipMaxResults
    , dipInstanceId

    -- * Destructuring the Response
    , describeInstancePatchesResponse
    , DescribeInstancePatchesResponse
    -- * Response Lenses
    , diprsPatches
    , diprsNextToken
    , diprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'describeInstancePatches' smart constructor.
data DescribeInstancePatches = DescribeInstancePatches'
  { _dipFilters    :: !(Maybe [PatchOrchestratorFilter])
  , _dipNextToken  :: !(Maybe Text)
  , _dipMaxResults :: !(Maybe Nat)
  , _dipInstanceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInstancePatches' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dipFilters' - Each entry in the array is a structure containing: Key (string, between 1 and 128 characters) Values (array of strings, each string between 1 and 256 characters)
--
-- * 'dipNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dipMaxResults' - The maximum number of patches to return (per page).
--
-- * 'dipInstanceId' - The ID of the instance whose patch state information should be retrieved.
describeInstancePatches
    :: Text -- ^ 'dipInstanceId'
    -> DescribeInstancePatches
describeInstancePatches pInstanceId_ =
  DescribeInstancePatches'
    { _dipFilters = Nothing
    , _dipNextToken = Nothing
    , _dipMaxResults = Nothing
    , _dipInstanceId = pInstanceId_
    }


-- | Each entry in the array is a structure containing: Key (string, between 1 and 128 characters) Values (array of strings, each string between 1 and 256 characters)
dipFilters :: Lens' DescribeInstancePatches [PatchOrchestratorFilter]
dipFilters = lens _dipFilters (\ s a -> s{_dipFilters = a}) . _Default . _Coerce

-- | The token for the next set of items to return. (You received this token from a previous call.)
dipNextToken :: Lens' DescribeInstancePatches (Maybe Text)
dipNextToken = lens _dipNextToken (\ s a -> s{_dipNextToken = a})

-- | The maximum number of patches to return (per page).
dipMaxResults :: Lens' DescribeInstancePatches (Maybe Natural)
dipMaxResults = lens _dipMaxResults (\ s a -> s{_dipMaxResults = a}) . mapping _Nat

-- | The ID of the instance whose patch state information should be retrieved.
dipInstanceId :: Lens' DescribeInstancePatches Text
dipInstanceId = lens _dipInstanceId (\ s a -> s{_dipInstanceId = a})

instance AWSRequest DescribeInstancePatches where
        type Rs DescribeInstancePatches =
             DescribeInstancePatchesResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribeInstancePatchesResponse' <$>
                   (x .?> "Patches" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeInstancePatches where

instance NFData DescribeInstancePatches where

instance ToHeaders DescribeInstancePatches where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribeInstancePatches" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeInstancePatches where
        toJSON DescribeInstancePatches'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _dipFilters,
                  ("NextToken" .=) <$> _dipNextToken,
                  ("MaxResults" .=) <$> _dipMaxResults,
                  Just ("InstanceId" .= _dipInstanceId)])

instance ToPath DescribeInstancePatches where
        toPath = const "/"

instance ToQuery DescribeInstancePatches where
        toQuery = const mempty

-- | /See:/ 'describeInstancePatchesResponse' smart constructor.
data DescribeInstancePatchesResponse = DescribeInstancePatchesResponse'
  { _diprsPatches        :: !(Maybe [PatchComplianceData])
  , _diprsNextToken      :: !(Maybe Text)
  , _diprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInstancePatchesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diprsPatches' - Each entry in the array is a structure containing: Title (string) KBId (string) Classification (string) Severity (string) State (string: "INSTALLED", "INSTALLED OTHER", "MISSING", "NOT APPLICABLE", "FAILED") InstalledTime (DateTime) InstalledBy (string)
--
-- * 'diprsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'diprsResponseStatus' - -- | The response status code.
describeInstancePatchesResponse
    :: Int -- ^ 'diprsResponseStatus'
    -> DescribeInstancePatchesResponse
describeInstancePatchesResponse pResponseStatus_ =
  DescribeInstancePatchesResponse'
    { _diprsPatches = Nothing
    , _diprsNextToken = Nothing
    , _diprsResponseStatus = pResponseStatus_
    }


-- | Each entry in the array is a structure containing: Title (string) KBId (string) Classification (string) Severity (string) State (string: "INSTALLED", "INSTALLED OTHER", "MISSING", "NOT APPLICABLE", "FAILED") InstalledTime (DateTime) InstalledBy (string)
diprsPatches :: Lens' DescribeInstancePatchesResponse [PatchComplianceData]
diprsPatches = lens _diprsPatches (\ s a -> s{_diprsPatches = a}) . _Default . _Coerce

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
diprsNextToken :: Lens' DescribeInstancePatchesResponse (Maybe Text)
diprsNextToken = lens _diprsNextToken (\ s a -> s{_diprsNextToken = a})

-- | -- | The response status code.
diprsResponseStatus :: Lens' DescribeInstancePatchesResponse Int
diprsResponseStatus = lens _diprsResponseStatus (\ s a -> s{_diprsResponseStatus = a})

instance NFData DescribeInstancePatchesResponse where
