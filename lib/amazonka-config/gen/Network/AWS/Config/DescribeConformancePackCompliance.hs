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
-- Module      : Network.AWS.Config.DescribeConformancePackCompliance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns compliance details for each rule in that conformance pack.
module Network.AWS.Config.DescribeConformancePackCompliance
  ( -- * Creating a Request
    describeConformancePackCompliance,
    DescribeConformancePackCompliance,

    -- * Request Lenses
    dcpcFilters,
    dcpcNextToken,
    dcpcLimit,
    dcpcConformancePackName,

    -- * Destructuring the Response
    describeConformancePackComplianceResponse,
    DescribeConformancePackComplianceResponse,

    -- * Response Lenses
    dcpcrsNextToken,
    dcpcrsResponseStatus,
    dcpcrsConformancePackName,
    dcpcrsConformancePackRuleComplianceList,
  )
where

import Network.AWS.Config.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeConformancePackCompliance' smart constructor.
data DescribeConformancePackCompliance = DescribeConformancePackCompliance'
  { _dcpcFilters ::
      !( Maybe
           ConformancePackComplianceFilters
       ),
    _dcpcNextToken ::
      !(Maybe Text),
    _dcpcLimit ::
      !(Maybe Nat),
    _dcpcConformancePackName ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeConformancePackCompliance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpcFilters' - A @ConformancePackComplianceFilters@ object.
--
-- * 'dcpcNextToken' - The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- * 'dcpcLimit' - The maximum number of AWS Config rules within a conformance pack are returned on each page.
--
-- * 'dcpcConformancePackName' - Name of the conformance pack.
describeConformancePackCompliance ::
  -- | 'dcpcConformancePackName'
  Text ->
  DescribeConformancePackCompliance
describeConformancePackCompliance pConformancePackName_ =
  DescribeConformancePackCompliance'
    { _dcpcFilters = Nothing,
      _dcpcNextToken = Nothing,
      _dcpcLimit = Nothing,
      _dcpcConformancePackName = pConformancePackName_
    }

-- | A @ConformancePackComplianceFilters@ object.
dcpcFilters :: Lens' DescribeConformancePackCompliance (Maybe ConformancePackComplianceFilters)
dcpcFilters = lens _dcpcFilters (\s a -> s {_dcpcFilters = a})

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
dcpcNextToken :: Lens' DescribeConformancePackCompliance (Maybe Text)
dcpcNextToken = lens _dcpcNextToken (\s a -> s {_dcpcNextToken = a})

-- | The maximum number of AWS Config rules within a conformance pack are returned on each page.
dcpcLimit :: Lens' DescribeConformancePackCompliance (Maybe Natural)
dcpcLimit = lens _dcpcLimit (\s a -> s {_dcpcLimit = a}) . mapping _Nat

-- | Name of the conformance pack.
dcpcConformancePackName :: Lens' DescribeConformancePackCompliance Text
dcpcConformancePackName = lens _dcpcConformancePackName (\s a -> s {_dcpcConformancePackName = a})

instance AWSRequest DescribeConformancePackCompliance where
  type
    Rs DescribeConformancePackCompliance =
      DescribeConformancePackComplianceResponse
  request = postJSON config
  response =
    receiveJSON
      ( \s h x ->
          DescribeConformancePackComplianceResponse'
            <$> (x .?> "NextToken")
            <*> (pure (fromEnum s))
            <*> (x .:> "ConformancePackName")
            <*> (x .?> "ConformancePackRuleComplianceList" .!@ mempty)
      )

instance Hashable DescribeConformancePackCompliance

instance NFData DescribeConformancePackCompliance

instance ToHeaders DescribeConformancePackCompliance where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "StarlingDoveService.DescribeConformancePackCompliance" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeConformancePackCompliance where
  toJSON DescribeConformancePackCompliance' {..} =
    object
      ( catMaybes
          [ ("Filters" .=) <$> _dcpcFilters,
            ("NextToken" .=) <$> _dcpcNextToken,
            ("Limit" .=) <$> _dcpcLimit,
            Just ("ConformancePackName" .= _dcpcConformancePackName)
          ]
      )

instance ToPath DescribeConformancePackCompliance where
  toPath = const "/"

instance ToQuery DescribeConformancePackCompliance where
  toQuery = const mempty

-- | /See:/ 'describeConformancePackComplianceResponse' smart constructor.
data DescribeConformancePackComplianceResponse = DescribeConformancePackComplianceResponse'
  { _dcpcrsNextToken ::
      !( Maybe
           Text
       ),
    _dcpcrsResponseStatus ::
      !Int,
    _dcpcrsConformancePackName ::
      !Text,
    _dcpcrsConformancePackRuleComplianceList ::
      ![ConformancePackRuleCompliance]
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DescribeConformancePackComplianceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpcrsNextToken' - The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- * 'dcpcrsResponseStatus' - -- | The response status code.
--
-- * 'dcpcrsConformancePackName' - Name of the conformance pack.
--
-- * 'dcpcrsConformancePackRuleComplianceList' - Returns a list of @ConformancePackRuleCompliance@ objects.
describeConformancePackComplianceResponse ::
  -- | 'dcpcrsResponseStatus'
  Int ->
  -- | 'dcpcrsConformancePackName'
  Text ->
  DescribeConformancePackComplianceResponse
describeConformancePackComplianceResponse
  pResponseStatus_
  pConformancePackName_ =
    DescribeConformancePackComplianceResponse'
      { _dcpcrsNextToken =
          Nothing,
        _dcpcrsResponseStatus = pResponseStatus_,
        _dcpcrsConformancePackName = pConformancePackName_,
        _dcpcrsConformancePackRuleComplianceList = mempty
      }

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
dcpcrsNextToken :: Lens' DescribeConformancePackComplianceResponse (Maybe Text)
dcpcrsNextToken = lens _dcpcrsNextToken (\s a -> s {_dcpcrsNextToken = a})

-- | -- | The response status code.
dcpcrsResponseStatus :: Lens' DescribeConformancePackComplianceResponse Int
dcpcrsResponseStatus = lens _dcpcrsResponseStatus (\s a -> s {_dcpcrsResponseStatus = a})

-- | Name of the conformance pack.
dcpcrsConformancePackName :: Lens' DescribeConformancePackComplianceResponse Text
dcpcrsConformancePackName = lens _dcpcrsConformancePackName (\s a -> s {_dcpcrsConformancePackName = a})

-- | Returns a list of @ConformancePackRuleCompliance@ objects.
dcpcrsConformancePackRuleComplianceList :: Lens' DescribeConformancePackComplianceResponse [ConformancePackRuleCompliance]
dcpcrsConformancePackRuleComplianceList = lens _dcpcrsConformancePackRuleComplianceList (\s a -> s {_dcpcrsConformancePackRuleComplianceList = a}) . _Coerce

instance NFData DescribeConformancePackComplianceResponse
