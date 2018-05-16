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
-- Module      : Network.AWS.SSM.DescribeEffectivePatchesForPatchBaseline
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current effective patches (the patch and the approval state) for the specified patch baseline. Note that this API applies only to Windows patch baselines.
--
--
module Network.AWS.SSM.DescribeEffectivePatchesForPatchBaseline
    (
    -- * Creating a Request
      describeEffectivePatchesForPatchBaseline
    , DescribeEffectivePatchesForPatchBaseline
    -- * Request Lenses
    , depfpbNextToken
    , depfpbMaxResults
    , depfpbBaselineId

    -- * Destructuring the Response
    , describeEffectivePatchesForPatchBaselineResponse
    , DescribeEffectivePatchesForPatchBaselineResponse
    -- * Response Lenses
    , depfpbrsEffectivePatches
    , depfpbrsNextToken
    , depfpbrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'describeEffectivePatchesForPatchBaseline' smart constructor.
data DescribeEffectivePatchesForPatchBaseline = DescribeEffectivePatchesForPatchBaseline'
  { _depfpbNextToken  :: !(Maybe Text)
  , _depfpbMaxResults :: !(Maybe Nat)
  , _depfpbBaselineId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEffectivePatchesForPatchBaseline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'depfpbNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'depfpbMaxResults' - The maximum number of patches to return (per page).
--
-- * 'depfpbBaselineId' - The ID of the patch baseline to retrieve the effective patches for.
describeEffectivePatchesForPatchBaseline
    :: Text -- ^ 'depfpbBaselineId'
    -> DescribeEffectivePatchesForPatchBaseline
describeEffectivePatchesForPatchBaseline pBaselineId_ =
  DescribeEffectivePatchesForPatchBaseline'
    { _depfpbNextToken = Nothing
    , _depfpbMaxResults = Nothing
    , _depfpbBaselineId = pBaselineId_
    }


-- | The token for the next set of items to return. (You received this token from a previous call.)
depfpbNextToken :: Lens' DescribeEffectivePatchesForPatchBaseline (Maybe Text)
depfpbNextToken = lens _depfpbNextToken (\ s a -> s{_depfpbNextToken = a})

-- | The maximum number of patches to return (per page).
depfpbMaxResults :: Lens' DescribeEffectivePatchesForPatchBaseline (Maybe Natural)
depfpbMaxResults = lens _depfpbMaxResults (\ s a -> s{_depfpbMaxResults = a}) . mapping _Nat

-- | The ID of the patch baseline to retrieve the effective patches for.
depfpbBaselineId :: Lens' DescribeEffectivePatchesForPatchBaseline Text
depfpbBaselineId = lens _depfpbBaselineId (\ s a -> s{_depfpbBaselineId = a})

instance AWSRequest
           DescribeEffectivePatchesForPatchBaseline
         where
        type Rs DescribeEffectivePatchesForPatchBaseline =
             DescribeEffectivePatchesForPatchBaselineResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEffectivePatchesForPatchBaselineResponse' <$>
                   (x .?> "EffectivePatches" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable
           DescribeEffectivePatchesForPatchBaseline
         where

instance NFData
           DescribeEffectivePatchesForPatchBaseline
         where

instance ToHeaders
           DescribeEffectivePatchesForPatchBaseline
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribeEffectivePatchesForPatchBaseline"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON
           DescribeEffectivePatchesForPatchBaseline
         where
        toJSON DescribeEffectivePatchesForPatchBaseline'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _depfpbNextToken,
                  ("MaxResults" .=) <$> _depfpbMaxResults,
                  Just ("BaselineId" .= _depfpbBaselineId)])

instance ToPath
           DescribeEffectivePatchesForPatchBaseline
         where
        toPath = const "/"

instance ToQuery
           DescribeEffectivePatchesForPatchBaseline
         where
        toQuery = const mempty

-- | /See:/ 'describeEffectivePatchesForPatchBaselineResponse' smart constructor.
data DescribeEffectivePatchesForPatchBaselineResponse = DescribeEffectivePatchesForPatchBaselineResponse'
  { _depfpbrsEffectivePatches :: !(Maybe [EffectivePatch])
  , _depfpbrsNextToken        :: !(Maybe Text)
  , _depfpbrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEffectivePatchesForPatchBaselineResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'depfpbrsEffectivePatches' - An array of patches and patch status.
--
-- * 'depfpbrsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'depfpbrsResponseStatus' - -- | The response status code.
describeEffectivePatchesForPatchBaselineResponse
    :: Int -- ^ 'depfpbrsResponseStatus'
    -> DescribeEffectivePatchesForPatchBaselineResponse
describeEffectivePatchesForPatchBaselineResponse pResponseStatus_ =
  DescribeEffectivePatchesForPatchBaselineResponse'
    { _depfpbrsEffectivePatches = Nothing
    , _depfpbrsNextToken = Nothing
    , _depfpbrsResponseStatus = pResponseStatus_
    }


-- | An array of patches and patch status.
depfpbrsEffectivePatches :: Lens' DescribeEffectivePatchesForPatchBaselineResponse [EffectivePatch]
depfpbrsEffectivePatches = lens _depfpbrsEffectivePatches (\ s a -> s{_depfpbrsEffectivePatches = a}) . _Default . _Coerce

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
depfpbrsNextToken :: Lens' DescribeEffectivePatchesForPatchBaselineResponse (Maybe Text)
depfpbrsNextToken = lens _depfpbrsNextToken (\ s a -> s{_depfpbrsNextToken = a})

-- | -- | The response status code.
depfpbrsResponseStatus :: Lens' DescribeEffectivePatchesForPatchBaselineResponse Int
depfpbrsResponseStatus = lens _depfpbrsResponseStatus (\ s a -> s{_depfpbrsResponseStatus = a})

instance NFData
           DescribeEffectivePatchesForPatchBaselineResponse
         where
