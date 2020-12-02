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
-- Module      : Network.AWS.Config.GetConformancePackComplianceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns compliance details for the conformance pack based on the cumulative compliance results of all the rules in that conformance pack.
module Network.AWS.Config.GetConformancePackComplianceSummary
  ( -- * Creating a Request
    getConformancePackComplianceSummary,
    GetConformancePackComplianceSummary,

    -- * Request Lenses
    gcpcsNextToken,
    gcpcsLimit,
    gcpcsConformancePackNames,

    -- * Destructuring the Response
    getConformancePackComplianceSummaryResponse,
    GetConformancePackComplianceSummaryResponse,

    -- * Response Lenses
    gcpcsrsConformancePackComplianceSummaryList,
    gcpcsrsNextToken,
    gcpcsrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getConformancePackComplianceSummary' smart constructor.
data GetConformancePackComplianceSummary = GetConformancePackComplianceSummary'
  { _gcpcsNextToken ::
      !(Maybe Text),
    _gcpcsLimit ::
      !(Maybe Nat),
    _gcpcsConformancePackNames ::
      !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetConformancePackComplianceSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcpcsNextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'gcpcsLimit' - The maximum number of conformance packs returned on each page.
--
-- * 'gcpcsConformancePackNames' - Names of conformance packs.
getConformancePackComplianceSummary ::
  -- | 'gcpcsConformancePackNames'
  NonEmpty Text ->
  GetConformancePackComplianceSummary
getConformancePackComplianceSummary pConformancePackNames_ =
  GetConformancePackComplianceSummary'
    { _gcpcsNextToken = Nothing,
      _gcpcsLimit = Nothing,
      _gcpcsConformancePackNames =
        _List1 # pConformancePackNames_
    }

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
gcpcsNextToken :: Lens' GetConformancePackComplianceSummary (Maybe Text)
gcpcsNextToken = lens _gcpcsNextToken (\s a -> s {_gcpcsNextToken = a})

-- | The maximum number of conformance packs returned on each page.
gcpcsLimit :: Lens' GetConformancePackComplianceSummary (Maybe Natural)
gcpcsLimit = lens _gcpcsLimit (\s a -> s {_gcpcsLimit = a}) . mapping _Nat

-- | Names of conformance packs.
gcpcsConformancePackNames :: Lens' GetConformancePackComplianceSummary (NonEmpty Text)
gcpcsConformancePackNames = lens _gcpcsConformancePackNames (\s a -> s {_gcpcsConformancePackNames = a}) . _List1

instance AWSRequest GetConformancePackComplianceSummary where
  type
    Rs GetConformancePackComplianceSummary =
      GetConformancePackComplianceSummaryResponse
  request = postJSON config
  response =
    receiveJSON
      ( \s h x ->
          GetConformancePackComplianceSummaryResponse'
            <$> (x .?> "ConformancePackComplianceSummaryList")
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable GetConformancePackComplianceSummary

instance NFData GetConformancePackComplianceSummary

instance ToHeaders GetConformancePackComplianceSummary where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "StarlingDoveService.GetConformancePackComplianceSummary" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetConformancePackComplianceSummary where
  toJSON GetConformancePackComplianceSummary' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _gcpcsNextToken,
            ("Limit" .=) <$> _gcpcsLimit,
            Just ("ConformancePackNames" .= _gcpcsConformancePackNames)
          ]
      )

instance ToPath GetConformancePackComplianceSummary where
  toPath = const "/"

instance ToQuery GetConformancePackComplianceSummary where
  toQuery = const mempty

-- | /See:/ 'getConformancePackComplianceSummaryResponse' smart constructor.
data GetConformancePackComplianceSummaryResponse = GetConformancePackComplianceSummaryResponse'
  { _gcpcsrsConformancePackComplianceSummaryList ::
      !( Maybe
           ( List1
               ConformancePackComplianceSummary
           )
       ),
    _gcpcsrsNextToken ::
      !( Maybe
           Text
       ),
    _gcpcsrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'GetConformancePackComplianceSummaryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcpcsrsConformancePackComplianceSummaryList' - A list of @ConformancePackComplianceSummary@ objects.
--
-- * 'gcpcsrsNextToken' - The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- * 'gcpcsrsResponseStatus' - -- | The response status code.
getConformancePackComplianceSummaryResponse ::
  -- | 'gcpcsrsResponseStatus'
  Int ->
  GetConformancePackComplianceSummaryResponse
getConformancePackComplianceSummaryResponse pResponseStatus_ =
  GetConformancePackComplianceSummaryResponse'
    { _gcpcsrsConformancePackComplianceSummaryList =
        Nothing,
      _gcpcsrsNextToken = Nothing,
      _gcpcsrsResponseStatus = pResponseStatus_
    }

-- | A list of @ConformancePackComplianceSummary@ objects.
gcpcsrsConformancePackComplianceSummaryList :: Lens' GetConformancePackComplianceSummaryResponse (Maybe (NonEmpty ConformancePackComplianceSummary))
gcpcsrsConformancePackComplianceSummaryList = lens _gcpcsrsConformancePackComplianceSummaryList (\s a -> s {_gcpcsrsConformancePackComplianceSummaryList = a}) . mapping _List1

-- | The nextToken string returned on a previous page that you use to get the next page of results in a paginated response.
gcpcsrsNextToken :: Lens' GetConformancePackComplianceSummaryResponse (Maybe Text)
gcpcsrsNextToken = lens _gcpcsrsNextToken (\s a -> s {_gcpcsrsNextToken = a})

-- | -- | The response status code.
gcpcsrsResponseStatus :: Lens' GetConformancePackComplianceSummaryResponse Int
gcpcsrsResponseStatus = lens _gcpcsrsResponseStatus (\s a -> s {_gcpcsrsResponseStatus = a})

instance NFData GetConformancePackComplianceSummaryResponse
