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
-- Module      : Network.AWS.SSM.DescribePatchGroupState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns high-level aggregated patch compliance state for a patch group.
--
--
module Network.AWS.SSM.DescribePatchGroupState
    (
    -- * Creating a Request
      describePatchGroupState
    , DescribePatchGroupState
    -- * Request Lenses
    , dpgsPatchGroup

    -- * Destructuring the Response
    , describePatchGroupStateResponse
    , DescribePatchGroupStateResponse
    -- * Response Lenses
    , dpgsrsInstancesWithMissingPatches
    , dpgsrsInstancesWithInstalledOtherPatches
    , dpgsrsInstancesWithNotApplicablePatches
    , dpgsrsInstancesWithInstalledPatches
    , dpgsrsInstances
    , dpgsrsInstancesWithFailedPatches
    , dpgsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'describePatchGroupState' smart constructor.
newtype DescribePatchGroupState = DescribePatchGroupState'
  { _dpgsPatchGroup :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePatchGroupState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpgsPatchGroup' - The name of the patch group whose patch snapshot should be retrieved.
describePatchGroupState
    :: Text -- ^ 'dpgsPatchGroup'
    -> DescribePatchGroupState
describePatchGroupState pPatchGroup_ =
  DescribePatchGroupState' {_dpgsPatchGroup = pPatchGroup_}


-- | The name of the patch group whose patch snapshot should be retrieved.
dpgsPatchGroup :: Lens' DescribePatchGroupState Text
dpgsPatchGroup = lens _dpgsPatchGroup (\ s a -> s{_dpgsPatchGroup = a})

instance AWSRequest DescribePatchGroupState where
        type Rs DescribePatchGroupState =
             DescribePatchGroupStateResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribePatchGroupStateResponse' <$>
                   (x .?> "InstancesWithMissingPatches") <*>
                     (x .?> "InstancesWithInstalledOtherPatches")
                     <*> (x .?> "InstancesWithNotApplicablePatches")
                     <*> (x .?> "InstancesWithInstalledPatches")
                     <*> (x .?> "Instances")
                     <*> (x .?> "InstancesWithFailedPatches")
                     <*> (pure (fromEnum s)))

instance Hashable DescribePatchGroupState where

instance NFData DescribePatchGroupState where

instance ToHeaders DescribePatchGroupState where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribePatchGroupState" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribePatchGroupState where
        toJSON DescribePatchGroupState'{..}
          = object
              (catMaybes [Just ("PatchGroup" .= _dpgsPatchGroup)])

instance ToPath DescribePatchGroupState where
        toPath = const "/"

instance ToQuery DescribePatchGroupState where
        toQuery = const mempty

-- | /See:/ 'describePatchGroupStateResponse' smart constructor.
data DescribePatchGroupStateResponse = DescribePatchGroupStateResponse'
  { _dpgsrsInstancesWithMissingPatches        :: !(Maybe Int)
  , _dpgsrsInstancesWithInstalledOtherPatches :: !(Maybe Int)
  , _dpgsrsInstancesWithNotApplicablePatches  :: !(Maybe Int)
  , _dpgsrsInstancesWithInstalledPatches      :: !(Maybe Int)
  , _dpgsrsInstances                          :: !(Maybe Int)
  , _dpgsrsInstancesWithFailedPatches         :: !(Maybe Int)
  , _dpgsrsResponseStatus                     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePatchGroupStateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpgsrsInstancesWithMissingPatches' - The number of instances with missing patches from the patch baseline.
--
-- * 'dpgsrsInstancesWithInstalledOtherPatches' - The number of instances with patches installed that aren't defined in the patch baseline.
--
-- * 'dpgsrsInstancesWithNotApplicablePatches' - The number of instances with patches that aren't applicable.
--
-- * 'dpgsrsInstancesWithInstalledPatches' - The number of instances with installed patches.
--
-- * 'dpgsrsInstances' - The number of instances in the patch group.
--
-- * 'dpgsrsInstancesWithFailedPatches' - The number of instances with patches from the patch baseline that failed to install.
--
-- * 'dpgsrsResponseStatus' - -- | The response status code.
describePatchGroupStateResponse
    :: Int -- ^ 'dpgsrsResponseStatus'
    -> DescribePatchGroupStateResponse
describePatchGroupStateResponse pResponseStatus_ =
  DescribePatchGroupStateResponse'
    { _dpgsrsInstancesWithMissingPatches = Nothing
    , _dpgsrsInstancesWithInstalledOtherPatches = Nothing
    , _dpgsrsInstancesWithNotApplicablePatches = Nothing
    , _dpgsrsInstancesWithInstalledPatches = Nothing
    , _dpgsrsInstances = Nothing
    , _dpgsrsInstancesWithFailedPatches = Nothing
    , _dpgsrsResponseStatus = pResponseStatus_
    }


-- | The number of instances with missing patches from the patch baseline.
dpgsrsInstancesWithMissingPatches :: Lens' DescribePatchGroupStateResponse (Maybe Int)
dpgsrsInstancesWithMissingPatches = lens _dpgsrsInstancesWithMissingPatches (\ s a -> s{_dpgsrsInstancesWithMissingPatches = a})

-- | The number of instances with patches installed that aren't defined in the patch baseline.
dpgsrsInstancesWithInstalledOtherPatches :: Lens' DescribePatchGroupStateResponse (Maybe Int)
dpgsrsInstancesWithInstalledOtherPatches = lens _dpgsrsInstancesWithInstalledOtherPatches (\ s a -> s{_dpgsrsInstancesWithInstalledOtherPatches = a})

-- | The number of instances with patches that aren't applicable.
dpgsrsInstancesWithNotApplicablePatches :: Lens' DescribePatchGroupStateResponse (Maybe Int)
dpgsrsInstancesWithNotApplicablePatches = lens _dpgsrsInstancesWithNotApplicablePatches (\ s a -> s{_dpgsrsInstancesWithNotApplicablePatches = a})

-- | The number of instances with installed patches.
dpgsrsInstancesWithInstalledPatches :: Lens' DescribePatchGroupStateResponse (Maybe Int)
dpgsrsInstancesWithInstalledPatches = lens _dpgsrsInstancesWithInstalledPatches (\ s a -> s{_dpgsrsInstancesWithInstalledPatches = a})

-- | The number of instances in the patch group.
dpgsrsInstances :: Lens' DescribePatchGroupStateResponse (Maybe Int)
dpgsrsInstances = lens _dpgsrsInstances (\ s a -> s{_dpgsrsInstances = a})

-- | The number of instances with patches from the patch baseline that failed to install.
dpgsrsInstancesWithFailedPatches :: Lens' DescribePatchGroupStateResponse (Maybe Int)
dpgsrsInstancesWithFailedPatches = lens _dpgsrsInstancesWithFailedPatches (\ s a -> s{_dpgsrsInstancesWithFailedPatches = a})

-- | -- | The response status code.
dpgsrsResponseStatus :: Lens' DescribePatchGroupStateResponse Int
dpgsrsResponseStatus = lens _dpgsrsResponseStatus (\ s a -> s{_dpgsrsResponseStatus = a})

instance NFData DescribePatchGroupStateResponse where
