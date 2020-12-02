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
-- Module      : Network.AWS.Config.GetConformancePackComplianceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns compliance details of a conformance pack for all AWS resources that are monitered by conformance pack.
module Network.AWS.Config.GetConformancePackComplianceDetails
  ( -- * Creating a Request
    getConformancePackComplianceDetails,
    GetConformancePackComplianceDetails,

    -- * Request Lenses
    gcpcdFilters,
    gcpcdNextToken,
    gcpcdLimit,
    gcpcdConformancePackName,

    -- * Destructuring the Response
    getConformancePackComplianceDetailsResponse,
    GetConformancePackComplianceDetailsResponse,

    -- * Response Lenses
    gcpcdrsNextToken,
    gcpcdrsConformancePackRuleEvaluationResults,
    gcpcdrsResponseStatus,
    gcpcdrsConformancePackName,
  )
where

import Network.AWS.Config.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getConformancePackComplianceDetails' smart constructor.
data GetConformancePackComplianceDetails = GetConformancePackComplianceDetails'
  { _gcpcdFilters ::
      !( Maybe
           ConformancePackEvaluationFilters
       ),
    _gcpcdNextToken ::
      !(Maybe Text),
    _gcpcdLimit ::
      !(Maybe Nat),
    _gcpcdConformancePackName ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetConformancePackComplianceDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcpcdFilters' - A @ConformancePackEvaluationFilters@ object.
--
-- * 'gcpcdNextToken' - The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- * 'gcpcdLimit' - The maximum number of evaluation results returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
--
-- * 'gcpcdConformancePackName' - Name of the conformance pack.
getConformancePackComplianceDetails ::
  -- | 'gcpcdConformancePackName'
  Text ->
  GetConformancePackComplianceDetails
getConformancePackComplianceDetails pConformancePackName_ =
  GetConformancePackComplianceDetails'
    { _gcpcdFilters = Nothing,
      _gcpcdNextToken = Nothing,
      _gcpcdLimit = Nothing,
      _gcpcdConformancePackName = pConformancePackName_
    }

-- | A @ConformancePackEvaluationFilters@ object.
gcpcdFilters :: Lens' GetConformancePackComplianceDetails (Maybe ConformancePackEvaluationFilters)
gcpcdFilters = lens _gcpcdFilters (\s a -> s {_gcpcdFilters = a})

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
gcpcdNextToken :: Lens' GetConformancePackComplianceDetails (Maybe Text)
gcpcdNextToken = lens _gcpcdNextToken (\s a -> s {_gcpcdNextToken = a})

-- | The maximum number of evaluation results returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
gcpcdLimit :: Lens' GetConformancePackComplianceDetails (Maybe Natural)
gcpcdLimit = lens _gcpcdLimit (\s a -> s {_gcpcdLimit = a}) . mapping _Nat

-- | Name of the conformance pack.
gcpcdConformancePackName :: Lens' GetConformancePackComplianceDetails Text
gcpcdConformancePackName = lens _gcpcdConformancePackName (\s a -> s {_gcpcdConformancePackName = a})

instance AWSRequest GetConformancePackComplianceDetails where
  type
    Rs GetConformancePackComplianceDetails =
      GetConformancePackComplianceDetailsResponse
  request = postJSON config
  response =
    receiveJSON
      ( \s h x ->
          GetConformancePackComplianceDetailsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "ConformancePackRuleEvaluationResults" .!@ mempty)
            <*> (pure (fromEnum s))
            <*> (x .:> "ConformancePackName")
      )

instance Hashable GetConformancePackComplianceDetails

instance NFData GetConformancePackComplianceDetails

instance ToHeaders GetConformancePackComplianceDetails where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "StarlingDoveService.GetConformancePackComplianceDetails" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetConformancePackComplianceDetails where
  toJSON GetConformancePackComplianceDetails' {..} =
    object
      ( catMaybes
          [ ("Filters" .=) <$> _gcpcdFilters,
            ("NextToken" .=) <$> _gcpcdNextToken,
            ("Limit" .=) <$> _gcpcdLimit,
            Just ("ConformancePackName" .= _gcpcdConformancePackName)
          ]
      )

instance ToPath GetConformancePackComplianceDetails where
  toPath = const "/"

instance ToQuery GetConformancePackComplianceDetails where
  toQuery = const mempty

-- | /See:/ 'getConformancePackComplianceDetailsResponse' smart constructor.
data GetConformancePackComplianceDetailsResponse = GetConformancePackComplianceDetailsResponse'
  { _gcpcdrsNextToken ::
      !( Maybe
           Text
       ),
    _gcpcdrsConformancePackRuleEvaluationResults ::
      !( Maybe
           [ConformancePackEvaluationResult]
       ),
    _gcpcdrsResponseStatus ::
      !Int,
    _gcpcdrsConformancePackName ::
      !Text
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'GetConformancePackComplianceDetailsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcpcdrsNextToken' - The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- * 'gcpcdrsConformancePackRuleEvaluationResults' - Returns a list of @ConformancePackEvaluationResult@ objects.
--
-- * 'gcpcdrsResponseStatus' - -- | The response status code.
--
-- * 'gcpcdrsConformancePackName' - Name of the conformance pack.
getConformancePackComplianceDetailsResponse ::
  -- | 'gcpcdrsResponseStatus'
  Int ->
  -- | 'gcpcdrsConformancePackName'
  Text ->
  GetConformancePackComplianceDetailsResponse
getConformancePackComplianceDetailsResponse
  pResponseStatus_
  pConformancePackName_ =
    GetConformancePackComplianceDetailsResponse'
      { _gcpcdrsNextToken =
          Nothing,
        _gcpcdrsConformancePackRuleEvaluationResults =
          Nothing,
        _gcpcdrsResponseStatus = pResponseStatus_,
        _gcpcdrsConformancePackName =
          pConformancePackName_
      }

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
gcpcdrsNextToken :: Lens' GetConformancePackComplianceDetailsResponse (Maybe Text)
gcpcdrsNextToken = lens _gcpcdrsNextToken (\s a -> s {_gcpcdrsNextToken = a})

-- | Returns a list of @ConformancePackEvaluationResult@ objects.
gcpcdrsConformancePackRuleEvaluationResults :: Lens' GetConformancePackComplianceDetailsResponse [ConformancePackEvaluationResult]
gcpcdrsConformancePackRuleEvaluationResults = lens _gcpcdrsConformancePackRuleEvaluationResults (\s a -> s {_gcpcdrsConformancePackRuleEvaluationResults = a}) . _Default . _Coerce

-- | -- | The response status code.
gcpcdrsResponseStatus :: Lens' GetConformancePackComplianceDetailsResponse Int
gcpcdrsResponseStatus = lens _gcpcdrsResponseStatus (\s a -> s {_gcpcdrsResponseStatus = a})

-- | Name of the conformance pack.
gcpcdrsConformancePackName :: Lens' GetConformancePackComplianceDetailsResponse Text
gcpcdrsConformancePackName = lens _gcpcdrsConformancePackName (\s a -> s {_gcpcdrsConformancePackName = a})

instance NFData GetConformancePackComplianceDetailsResponse
