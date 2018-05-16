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
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindowTargets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the targets registered with the Maintenance Window.
--
--
module Network.AWS.SSM.DescribeMaintenanceWindowTargets
    (
    -- * Creating a Request
      describeMaintenanceWindowTargets
    , DescribeMaintenanceWindowTargets
    -- * Request Lenses
    , dmwtFilters
    , dmwtNextToken
    , dmwtMaxResults
    , dmwtWindowId

    -- * Destructuring the Response
    , describeMaintenanceWindowTargetsResponse
    , DescribeMaintenanceWindowTargetsResponse
    -- * Response Lenses
    , dmwtrsNextToken
    , dmwtrsTargets
    , dmwtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'describeMaintenanceWindowTargets' smart constructor.
data DescribeMaintenanceWindowTargets = DescribeMaintenanceWindowTargets'
  { _dmwtFilters    :: !(Maybe [MaintenanceWindowFilter])
  , _dmwtNextToken  :: !(Maybe Text)
  , _dmwtMaxResults :: !(Maybe Nat)
  , _dmwtWindowId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMaintenanceWindowTargets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmwtFilters' - Optional filters that can be used to narrow down the scope of the returned window targets. The supported filter keys are Type, WindowTargetId and OwnerInformation.
--
-- * 'dmwtNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dmwtMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- * 'dmwtWindowId' - The ID of the Maintenance Window whose targets should be retrieved.
describeMaintenanceWindowTargets
    :: Text -- ^ 'dmwtWindowId'
    -> DescribeMaintenanceWindowTargets
describeMaintenanceWindowTargets pWindowId_ =
  DescribeMaintenanceWindowTargets'
    { _dmwtFilters = Nothing
    , _dmwtNextToken = Nothing
    , _dmwtMaxResults = Nothing
    , _dmwtWindowId = pWindowId_
    }


-- | Optional filters that can be used to narrow down the scope of the returned window targets. The supported filter keys are Type, WindowTargetId and OwnerInformation.
dmwtFilters :: Lens' DescribeMaintenanceWindowTargets [MaintenanceWindowFilter]
dmwtFilters = lens _dmwtFilters (\ s a -> s{_dmwtFilters = a}) . _Default . _Coerce

-- | The token for the next set of items to return. (You received this token from a previous call.)
dmwtNextToken :: Lens' DescribeMaintenanceWindowTargets (Maybe Text)
dmwtNextToken = lens _dmwtNextToken (\ s a -> s{_dmwtNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
dmwtMaxResults :: Lens' DescribeMaintenanceWindowTargets (Maybe Natural)
dmwtMaxResults = lens _dmwtMaxResults (\ s a -> s{_dmwtMaxResults = a}) . mapping _Nat

-- | The ID of the Maintenance Window whose targets should be retrieved.
dmwtWindowId :: Lens' DescribeMaintenanceWindowTargets Text
dmwtWindowId = lens _dmwtWindowId (\ s a -> s{_dmwtWindowId = a})

instance AWSRequest DescribeMaintenanceWindowTargets
         where
        type Rs DescribeMaintenanceWindowTargets =
             DescribeMaintenanceWindowTargetsResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribeMaintenanceWindowTargetsResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Targets" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeMaintenanceWindowTargets
         where

instance NFData DescribeMaintenanceWindowTargets
         where

instance ToHeaders DescribeMaintenanceWindowTargets
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribeMaintenanceWindowTargets" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeMaintenanceWindowTargets
         where
        toJSON DescribeMaintenanceWindowTargets'{..}
          = object
              (catMaybes
                 [("Filters" .=) <$> _dmwtFilters,
                  ("NextToken" .=) <$> _dmwtNextToken,
                  ("MaxResults" .=) <$> _dmwtMaxResults,
                  Just ("WindowId" .= _dmwtWindowId)])

instance ToPath DescribeMaintenanceWindowTargets
         where
        toPath = const "/"

instance ToQuery DescribeMaintenanceWindowTargets
         where
        toQuery = const mempty

-- | /See:/ 'describeMaintenanceWindowTargetsResponse' smart constructor.
data DescribeMaintenanceWindowTargetsResponse = DescribeMaintenanceWindowTargetsResponse'
  { _dmwtrsNextToken      :: !(Maybe Text)
  , _dmwtrsTargets        :: !(Maybe [MaintenanceWindowTarget])
  , _dmwtrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMaintenanceWindowTargetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmwtrsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'dmwtrsTargets' - Information about the targets in the Maintenance Window.
--
-- * 'dmwtrsResponseStatus' - -- | The response status code.
describeMaintenanceWindowTargetsResponse
    :: Int -- ^ 'dmwtrsResponseStatus'
    -> DescribeMaintenanceWindowTargetsResponse
describeMaintenanceWindowTargetsResponse pResponseStatus_ =
  DescribeMaintenanceWindowTargetsResponse'
    { _dmwtrsNextToken = Nothing
    , _dmwtrsTargets = Nothing
    , _dmwtrsResponseStatus = pResponseStatus_
    }


-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
dmwtrsNextToken :: Lens' DescribeMaintenanceWindowTargetsResponse (Maybe Text)
dmwtrsNextToken = lens _dmwtrsNextToken (\ s a -> s{_dmwtrsNextToken = a})

-- | Information about the targets in the Maintenance Window.
dmwtrsTargets :: Lens' DescribeMaintenanceWindowTargetsResponse [MaintenanceWindowTarget]
dmwtrsTargets = lens _dmwtrsTargets (\ s a -> s{_dmwtrsTargets = a}) . _Default . _Coerce

-- | -- | The response status code.
dmwtrsResponseStatus :: Lens' DescribeMaintenanceWindowTargetsResponse Int
dmwtrsResponseStatus = lens _dmwtrsResponseStatus (\ s a -> s{_dmwtrsResponseStatus = a})

instance NFData
           DescribeMaintenanceWindowTargetsResponse
         where
