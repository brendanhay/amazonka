{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetFindingsStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists Amazon GuardDuty findings statistics for the specified detector ID.
module Network.AWS.GuardDuty.GetFindingsStatistics
  ( -- * Creating a Request
    getFindingsStatistics,
    GetFindingsStatistics,

    -- * Request Lenses
    gfsFindingCriteria,
    gfsDetectorId,
    gfsFindingStatisticTypes,

    -- * Destructuring the Response
    getFindingsStatisticsResponse,
    GetFindingsStatisticsResponse,

    -- * Response Lenses
    gfsrsResponseStatus,
    gfsrsFindingStatistics,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getFindingsStatistics' smart constructor.
data GetFindingsStatistics = GetFindingsStatistics'
  { _gfsFindingCriteria ::
      !(Maybe FindingCriteria),
    _gfsDetectorId :: !Text,
    _gfsFindingStatisticTypes ::
      ![FindingStatisticType]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetFindingsStatistics' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfsFindingCriteria' - Represents the criteria that is used for querying findings.
--
-- * 'gfsDetectorId' - The ID of the detector that specifies the GuardDuty service whose findings' statistics you want to retrieve.
--
-- * 'gfsFindingStatisticTypes' - The types of finding statistics to retrieve.
getFindingsStatistics ::
  -- | 'gfsDetectorId'
  Text ->
  GetFindingsStatistics
getFindingsStatistics pDetectorId_ =
  GetFindingsStatistics'
    { _gfsFindingCriteria = Nothing,
      _gfsDetectorId = pDetectorId_,
      _gfsFindingStatisticTypes = mempty
    }

-- | Represents the criteria that is used for querying findings.
gfsFindingCriteria :: Lens' GetFindingsStatistics (Maybe FindingCriteria)
gfsFindingCriteria = lens _gfsFindingCriteria (\s a -> s {_gfsFindingCriteria = a})

-- | The ID of the detector that specifies the GuardDuty service whose findings' statistics you want to retrieve.
gfsDetectorId :: Lens' GetFindingsStatistics Text
gfsDetectorId = lens _gfsDetectorId (\s a -> s {_gfsDetectorId = a})

-- | The types of finding statistics to retrieve.
gfsFindingStatisticTypes :: Lens' GetFindingsStatistics [FindingStatisticType]
gfsFindingStatisticTypes = lens _gfsFindingStatisticTypes (\s a -> s {_gfsFindingStatisticTypes = a}) . _Coerce

instance AWSRequest GetFindingsStatistics where
  type Rs GetFindingsStatistics = GetFindingsStatisticsResponse
  request = postJSON guardDuty
  response =
    receiveJSON
      ( \s h x ->
          GetFindingsStatisticsResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "findingStatistics")
      )

instance Hashable GetFindingsStatistics

instance NFData GetFindingsStatistics

instance ToHeaders GetFindingsStatistics where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON GetFindingsStatistics where
  toJSON GetFindingsStatistics' {..} =
    object
      ( catMaybes
          [ ("findingCriteria" .=) <$> _gfsFindingCriteria,
            Just ("findingStatisticTypes" .= _gfsFindingStatisticTypes)
          ]
      )

instance ToPath GetFindingsStatistics where
  toPath GetFindingsStatistics' {..} =
    mconcat
      ["/detector/", toBS _gfsDetectorId, "/findings/statistics"]

instance ToQuery GetFindingsStatistics where
  toQuery = const mempty

-- | /See:/ 'getFindingsStatisticsResponse' smart constructor.
data GetFindingsStatisticsResponse = GetFindingsStatisticsResponse'
  { _gfsrsResponseStatus ::
      !Int,
    _gfsrsFindingStatistics ::
      !FindingStatistics
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetFindingsStatisticsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfsrsResponseStatus' - -- | The response status code.
--
-- * 'gfsrsFindingStatistics' - The finding statistics object.
getFindingsStatisticsResponse ::
  -- | 'gfsrsResponseStatus'
  Int ->
  -- | 'gfsrsFindingStatistics'
  FindingStatistics ->
  GetFindingsStatisticsResponse
getFindingsStatisticsResponse pResponseStatus_ pFindingStatistics_ =
  GetFindingsStatisticsResponse'
    { _gfsrsResponseStatus =
        pResponseStatus_,
      _gfsrsFindingStatistics = pFindingStatistics_
    }

-- | -- | The response status code.
gfsrsResponseStatus :: Lens' GetFindingsStatisticsResponse Int
gfsrsResponseStatus = lens _gfsrsResponseStatus (\s a -> s {_gfsrsResponseStatus = a})

-- | The finding statistics object.
gfsrsFindingStatistics :: Lens' GetFindingsStatisticsResponse FindingStatistics
gfsrsFindingStatistics = lens _gfsrsFindingStatistics (\s a -> s {_gfsrsFindingStatistics = a})

instance NFData GetFindingsStatisticsResponse
