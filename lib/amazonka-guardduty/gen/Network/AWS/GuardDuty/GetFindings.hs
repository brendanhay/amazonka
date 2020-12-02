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
-- Module      : Network.AWS.GuardDuty.GetFindings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes Amazon GuardDuty findings specified by finding IDs.
module Network.AWS.GuardDuty.GetFindings
  ( -- * Creating a Request
    getFindings,
    GetFindings,

    -- * Request Lenses
    gfSortCriteria,
    gfDetectorId,
    gfFindingIds,

    -- * Destructuring the Response
    getFindingsResponse,
    GetFindingsResponse,

    -- * Response Lenses
    grsResponseStatus,
    grsFindings,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getFindings' smart constructor.
data GetFindings = GetFindings'
  { _gfSortCriteria ::
      !(Maybe SortCriteria),
    _gfDetectorId :: !Text,
    _gfFindingIds :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetFindings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfSortCriteria' - Represents the criteria used for sorting findings.
--
-- * 'gfDetectorId' - The ID of the detector that specifies the GuardDuty service whose findings you want to retrieve.
--
-- * 'gfFindingIds' - The IDs of the findings that you want to retrieve.
getFindings ::
  -- | 'gfDetectorId'
  Text ->
  GetFindings
getFindings pDetectorId_ =
  GetFindings'
    { _gfSortCriteria = Nothing,
      _gfDetectorId = pDetectorId_,
      _gfFindingIds = mempty
    }

-- | Represents the criteria used for sorting findings.
gfSortCriteria :: Lens' GetFindings (Maybe SortCriteria)
gfSortCriteria = lens _gfSortCriteria (\s a -> s {_gfSortCriteria = a})

-- | The ID of the detector that specifies the GuardDuty service whose findings you want to retrieve.
gfDetectorId :: Lens' GetFindings Text
gfDetectorId = lens _gfDetectorId (\s a -> s {_gfDetectorId = a})

-- | The IDs of the findings that you want to retrieve.
gfFindingIds :: Lens' GetFindings [Text]
gfFindingIds = lens _gfFindingIds (\s a -> s {_gfFindingIds = a}) . _Coerce

instance AWSRequest GetFindings where
  type Rs GetFindings = GetFindingsResponse
  request = postJSON guardDuty
  response =
    receiveJSON
      ( \s h x ->
          GetFindingsResponse'
            <$> (pure (fromEnum s)) <*> (x .?> "findings" .!@ mempty)
      )

instance Hashable GetFindings

instance NFData GetFindings

instance ToHeaders GetFindings where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON GetFindings where
  toJSON GetFindings' {..} =
    object
      ( catMaybes
          [ ("sortCriteria" .=) <$> _gfSortCriteria,
            Just ("findingIds" .= _gfFindingIds)
          ]
      )

instance ToPath GetFindings where
  toPath GetFindings' {..} =
    mconcat ["/detector/", toBS _gfDetectorId, "/findings/get"]

instance ToQuery GetFindings where
  toQuery = const mempty

-- | /See:/ 'getFindingsResponse' smart constructor.
data GetFindingsResponse = GetFindingsResponse'
  { _grsResponseStatus ::
      !Int,
    _grsFindings :: ![Finding]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetFindingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grsResponseStatus' - -- | The response status code.
--
-- * 'grsFindings' - A list of findings.
getFindingsResponse ::
  -- | 'grsResponseStatus'
  Int ->
  GetFindingsResponse
getFindingsResponse pResponseStatus_ =
  GetFindingsResponse'
    { _grsResponseStatus = pResponseStatus_,
      _grsFindings = mempty
    }

-- | -- | The response status code.
grsResponseStatus :: Lens' GetFindingsResponse Int
grsResponseStatus = lens _grsResponseStatus (\s a -> s {_grsResponseStatus = a})

-- | A list of findings.
grsFindings :: Lens' GetFindingsResponse [Finding]
grsFindings = lens _grsFindings (\s a -> s {_grsFindings = a}) . _Coerce

instance NFData GetFindingsResponse
