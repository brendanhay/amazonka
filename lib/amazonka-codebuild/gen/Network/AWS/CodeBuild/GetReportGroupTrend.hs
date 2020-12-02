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
-- Module      : Network.AWS.CodeBuild.GetReportGroupTrend
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.CodeBuild.GetReportGroupTrend
  ( -- * Creating a Request
    getReportGroupTrend,
    GetReportGroupTrend,

    -- * Request Lenses
    grgtNumOfReports,
    grgtReportGroupARN,
    grgtTrendField,

    -- * Destructuring the Response
    getReportGroupTrendResponse,
    GetReportGroupTrendResponse,

    -- * Response Lenses
    grgtrsRawData,
    grgtrsStats,
    grgtrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getReportGroupTrend' smart constructor.
data GetReportGroupTrend = GetReportGroupTrend'
  { _grgtNumOfReports ::
      !(Maybe Nat),
    _grgtReportGroupARN :: !Text,
    _grgtTrendField :: !ReportGroupTrendFieldType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetReportGroupTrend' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grgtNumOfReports' - Undocumented member.
--
-- * 'grgtReportGroupARN' - Undocumented member.
--
-- * 'grgtTrendField' - Undocumented member.
getReportGroupTrend ::
  -- | 'grgtReportGroupARN'
  Text ->
  -- | 'grgtTrendField'
  ReportGroupTrendFieldType ->
  GetReportGroupTrend
getReportGroupTrend pReportGroupARN_ pTrendField_ =
  GetReportGroupTrend'
    { _grgtNumOfReports = Nothing,
      _grgtReportGroupARN = pReportGroupARN_,
      _grgtTrendField = pTrendField_
    }

-- | Undocumented member.
grgtNumOfReports :: Lens' GetReportGroupTrend (Maybe Natural)
grgtNumOfReports = lens _grgtNumOfReports (\s a -> s {_grgtNumOfReports = a}) . mapping _Nat

-- | Undocumented member.
grgtReportGroupARN :: Lens' GetReportGroupTrend Text
grgtReportGroupARN = lens _grgtReportGroupARN (\s a -> s {_grgtReportGroupARN = a})

-- | Undocumented member.
grgtTrendField :: Lens' GetReportGroupTrend ReportGroupTrendFieldType
grgtTrendField = lens _grgtTrendField (\s a -> s {_grgtTrendField = a})

instance AWSRequest GetReportGroupTrend where
  type Rs GetReportGroupTrend = GetReportGroupTrendResponse
  request = postJSON codeBuild
  response =
    receiveJSON
      ( \s h x ->
          GetReportGroupTrendResponse'
            <$> (x .?> "rawData" .!@ mempty)
            <*> (x .?> "stats")
            <*> (pure (fromEnum s))
      )

instance Hashable GetReportGroupTrend

instance NFData GetReportGroupTrend

instance ToHeaders GetReportGroupTrend where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeBuild_20161006.GetReportGroupTrend" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetReportGroupTrend where
  toJSON GetReportGroupTrend' {..} =
    object
      ( catMaybes
          [ ("numOfReports" .=) <$> _grgtNumOfReports,
            Just ("reportGroupArn" .= _grgtReportGroupARN),
            Just ("trendField" .= _grgtTrendField)
          ]
      )

instance ToPath GetReportGroupTrend where
  toPath = const "/"

instance ToQuery GetReportGroupTrend where
  toQuery = const mempty

-- | /See:/ 'getReportGroupTrendResponse' smart constructor.
data GetReportGroupTrendResponse = GetReportGroupTrendResponse'
  { _grgtrsRawData ::
      !(Maybe [ReportWithRawData]),
    _grgtrsStats ::
      !(Maybe ReportGroupTrendStats),
    _grgtrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetReportGroupTrendResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grgtrsRawData' - Undocumented member.
--
-- * 'grgtrsStats' - Undocumented member.
--
-- * 'grgtrsResponseStatus' - -- | The response status code.
getReportGroupTrendResponse ::
  -- | 'grgtrsResponseStatus'
  Int ->
  GetReportGroupTrendResponse
getReportGroupTrendResponse pResponseStatus_ =
  GetReportGroupTrendResponse'
    { _grgtrsRawData = Nothing,
      _grgtrsStats = Nothing,
      _grgtrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
grgtrsRawData :: Lens' GetReportGroupTrendResponse [ReportWithRawData]
grgtrsRawData = lens _grgtrsRawData (\s a -> s {_grgtrsRawData = a}) . _Default . _Coerce

-- | Undocumented member.
grgtrsStats :: Lens' GetReportGroupTrendResponse (Maybe ReportGroupTrendStats)
grgtrsStats = lens _grgtrsStats (\s a -> s {_grgtrsStats = a})

-- | -- | The response status code.
grgtrsResponseStatus :: Lens' GetReportGroupTrendResponse Int
grgtrsResponseStatus = lens _grgtrsResponseStatus (\s a -> s {_grgtrsResponseStatus = a})

instance NFData GetReportGroupTrendResponse
