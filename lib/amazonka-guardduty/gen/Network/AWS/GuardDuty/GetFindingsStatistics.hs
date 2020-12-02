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
-- Module      : Network.AWS.GuardDuty.GetFindingsStatistics
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists Amazon GuardDuty findings' statistics for the specified detector ID.
module Network.AWS.GuardDuty.GetFindingsStatistics
    (
    -- * Creating a Request
      getFindingsStatistics
    , GetFindingsStatistics
    -- * Request Lenses
    , gfsFindingStatisticTypes
    , gfsFindingCriteria
    , gfsDetectorId

    -- * Destructuring the Response
    , getFindingsStatisticsResponse
    , GetFindingsStatisticsResponse
    -- * Response Lenses
    , gfsrsFindingStatistics
    , gfsrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | GetFindingsStatistics request body.
--
-- /See:/ 'getFindingsStatistics' smart constructor.
data GetFindingsStatistics = GetFindingsStatistics'
  { _gfsFindingStatisticTypes :: !(Maybe [FindingStatisticType])
  , _gfsFindingCriteria       :: !(Maybe FindingCriteria)
  , _gfsDetectorId            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFindingsStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfsFindingStatisticTypes' - Types of finding statistics to retrieve.
--
-- * 'gfsFindingCriteria' - Represents the criteria used for querying findings.
--
-- * 'gfsDetectorId' - The ID of the detector that specifies the GuardDuty service whose findings' statistics you want to retrieve.
getFindingsStatistics
    :: Text -- ^ 'gfsDetectorId'
    -> GetFindingsStatistics
getFindingsStatistics pDetectorId_ =
  GetFindingsStatistics'
    { _gfsFindingStatisticTypes = Nothing
    , _gfsFindingCriteria = Nothing
    , _gfsDetectorId = pDetectorId_
    }


-- | Types of finding statistics to retrieve.
gfsFindingStatisticTypes :: Lens' GetFindingsStatistics [FindingStatisticType]
gfsFindingStatisticTypes = lens _gfsFindingStatisticTypes (\ s a -> s{_gfsFindingStatisticTypes = a}) . _Default . _Coerce

-- | Represents the criteria used for querying findings.
gfsFindingCriteria :: Lens' GetFindingsStatistics (Maybe FindingCriteria)
gfsFindingCriteria = lens _gfsFindingCriteria (\ s a -> s{_gfsFindingCriteria = a})

-- | The ID of the detector that specifies the GuardDuty service whose findings' statistics you want to retrieve.
gfsDetectorId :: Lens' GetFindingsStatistics Text
gfsDetectorId = lens _gfsDetectorId (\ s a -> s{_gfsDetectorId = a})

instance AWSRequest GetFindingsStatistics where
        type Rs GetFindingsStatistics =
             GetFindingsStatisticsResponse
        request = postJSON guardDuty
        response
          = receiveJSON
              (\ s h x ->
                 GetFindingsStatisticsResponse' <$>
                   (x .?> "findingStatistics") <*> (pure (fromEnum s)))

instance Hashable GetFindingsStatistics where

instance NFData GetFindingsStatistics where

instance ToHeaders GetFindingsStatistics where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetFindingsStatistics where
        toJSON GetFindingsStatistics'{..}
          = object
              (catMaybes
                 [("findingStatisticTypes" .=) <$>
                    _gfsFindingStatisticTypes,
                  ("findingCriteria" .=) <$> _gfsFindingCriteria])

instance ToPath GetFindingsStatistics where
        toPath GetFindingsStatistics'{..}
          = mconcat
              ["/detector/", toBS _gfsDetectorId,
               "/findings/statistics"]

instance ToQuery GetFindingsStatistics where
        toQuery = const mempty

-- | /See:/ 'getFindingsStatisticsResponse' smart constructor.
data GetFindingsStatisticsResponse = GetFindingsStatisticsResponse'
  { _gfsrsFindingStatistics :: !(Maybe FindingStatistics)
  , _gfsrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFindingsStatisticsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfsrsFindingStatistics' - Finding statistics object.
--
-- * 'gfsrsResponseStatus' - -- | The response status code.
getFindingsStatisticsResponse
    :: Int -- ^ 'gfsrsResponseStatus'
    -> GetFindingsStatisticsResponse
getFindingsStatisticsResponse pResponseStatus_ =
  GetFindingsStatisticsResponse'
    {_gfsrsFindingStatistics = Nothing, _gfsrsResponseStatus = pResponseStatus_}


-- | Finding statistics object.
gfsrsFindingStatistics :: Lens' GetFindingsStatisticsResponse (Maybe FindingStatistics)
gfsrsFindingStatistics = lens _gfsrsFindingStatistics (\ s a -> s{_gfsrsFindingStatistics = a})

-- | -- | The response status code.
gfsrsResponseStatus :: Lens' GetFindingsStatisticsResponse Int
gfsrsResponseStatus = lens _gfsrsResponseStatus (\ s a -> s{_gfsrsResponseStatus = a})

instance NFData GetFindingsStatisticsResponse where
