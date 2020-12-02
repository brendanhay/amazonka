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
-- Module      : Network.AWS.SSM.DescribePatchBaselines
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the patch baselines in your AWS account.
--
--
module Network.AWS.SSM.DescribePatchBaselines
    (
    -- * Creating a Request
      describePatchBaselines
    , DescribePatchBaselines
    -- * Request Lenses
    , dpbFilters
    , dpbNextToken
    , dpbMaxResults

    -- * Destructuring the Response
    , describePatchBaselinesResponse
    , DescribePatchBaselinesResponse
    -- * Response Lenses
    , dpbsrsBaselineIdentities
    , dpbsrsNextToken
    , dpbsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'describePatchBaselines' smart constructor.
data DescribePatchBaselines = DescribePatchBaselines'
  { _dpbFilters    :: !(Maybe [PatchOrchestratorFilter])
  , _dpbNextToken  :: !(Maybe Text)
  , _dpbMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePatchBaselines' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpbFilters' - Each element in the array is a structure containing:  Key: (string, "NAME_PREFIX" or "OWNER") Value: (array of strings, exactly 1 entry, between 1 and 255 characters)
--
-- * 'dpbNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dpbMaxResults' - The maximum number of patch baselines to return (per page).
describePatchBaselines
    :: DescribePatchBaselines
describePatchBaselines =
  DescribePatchBaselines'
    {_dpbFilters = Nothing, _dpbNextToken = Nothing, _dpbMaxResults = Nothing}


-- | Each element in the array is a structure containing:  Key: (string, "NAME_PREFIX" or "OWNER") Value: (array of strings, exactly 1 entry, between 1 and 255 characters)
dpbFilters :: Lens' DescribePatchBaselines [PatchOrchestratorFilter]
dpbFilters = lens _dpbFilters (\ s a -> s{_dpbFilters = a}) . _Default . _Coerce

-- | The token for the next set of items to return. (You received this token from a previous call.)
dpbNextToken :: Lens' DescribePatchBaselines (Maybe Text)
dpbNextToken = lens _dpbNextToken (\ s a -> s{_dpbNextToken = a})

-- | The maximum number of patch baselines to return (per page).
dpbMaxResults :: Lens' DescribePatchBaselines (Maybe Natural)
dpbMaxResults = lens _dpbMaxResults (\ s a -> s{_dpbMaxResults = a}) . mapping _Nat

instance AWSRequest DescribePatchBaselines where
        type Rs DescribePatchBaselines =
             DescribePatchBaselinesResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribePatchBaselinesResponse' <$>
                   (x .?> "BaselineIdentities" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribePatchBaselines where

instance NFData DescribePatchBaselines where

instance ToHeaders DescribePatchBaselines where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribePatchBaselines" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribePatchBaselines where
        toJSON DescribePatchBaselines'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _dpbFilters,
                  ("NextToken" .=) <$> _dpbNextToken,
                  ("MaxResults" .=) <$> _dpbMaxResults])

instance ToPath DescribePatchBaselines where
        toPath = const "/"

instance ToQuery DescribePatchBaselines where
        toQuery = const mempty

-- | /See:/ 'describePatchBaselinesResponse' smart constructor.
data DescribePatchBaselinesResponse = DescribePatchBaselinesResponse'
  { _dpbsrsBaselineIdentities :: !(Maybe [PatchBaselineIdentity])
  , _dpbsrsNextToken          :: !(Maybe Text)
  , _dpbsrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePatchBaselinesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpbsrsBaselineIdentities' - An array of PatchBaselineIdentity elements.
--
-- * 'dpbsrsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'dpbsrsResponseStatus' - -- | The response status code.
describePatchBaselinesResponse
    :: Int -- ^ 'dpbsrsResponseStatus'
    -> DescribePatchBaselinesResponse
describePatchBaselinesResponse pResponseStatus_ =
  DescribePatchBaselinesResponse'
    { _dpbsrsBaselineIdentities = Nothing
    , _dpbsrsNextToken = Nothing
    , _dpbsrsResponseStatus = pResponseStatus_
    }


-- | An array of PatchBaselineIdentity elements.
dpbsrsBaselineIdentities :: Lens' DescribePatchBaselinesResponse [PatchBaselineIdentity]
dpbsrsBaselineIdentities = lens _dpbsrsBaselineIdentities (\ s a -> s{_dpbsrsBaselineIdentities = a}) . _Default . _Coerce

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
dpbsrsNextToken :: Lens' DescribePatchBaselinesResponse (Maybe Text)
dpbsrsNextToken = lens _dpbsrsNextToken (\ s a -> s{_dpbsrsNextToken = a})

-- | -- | The response status code.
dpbsrsResponseStatus :: Lens' DescribePatchBaselinesResponse Int
dpbsrsResponseStatus = lens _dpbsrsResponseStatus (\ s a -> s{_dpbsrsResponseStatus = a})

instance NFData DescribePatchBaselinesResponse where
