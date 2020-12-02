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
-- Module      : Network.AWS.Config.DescribeConformancePackStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides one or more conformance packs deployment status.
module Network.AWS.Config.DescribeConformancePackStatus
  ( -- * Creating a Request
    describeConformancePackStatus,
    DescribeConformancePackStatus,

    -- * Request Lenses
    dcpsConformancePackNames,
    dcpsNextToken,
    dcpsLimit,

    -- * Destructuring the Response
    describeConformancePackStatusResponse,
    DescribeConformancePackStatusResponse,

    -- * Response Lenses
    dcpsrsConformancePackStatusDetails,
    dcpsrsNextToken,
    dcpsrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeConformancePackStatus' smart constructor.
data DescribeConformancePackStatus = DescribeConformancePackStatus'
  { _dcpsConformancePackNames ::
      !(Maybe [Text]),
    _dcpsNextToken :: !(Maybe Text),
    _dcpsLimit :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeConformancePackStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpsConformancePackNames' - Comma-separated list of conformance pack names.
--
-- * 'dcpsNextToken' - The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- * 'dcpsLimit' - The maximum number of conformance packs status returned on each page.
describeConformancePackStatus ::
  DescribeConformancePackStatus
describeConformancePackStatus =
  DescribeConformancePackStatus'
    { _dcpsConformancePackNames =
        Nothing,
      _dcpsNextToken = Nothing,
      _dcpsLimit = Nothing
    }

-- | Comma-separated list of conformance pack names.
dcpsConformancePackNames :: Lens' DescribeConformancePackStatus [Text]
dcpsConformancePackNames = lens _dcpsConformancePackNames (\s a -> s {_dcpsConformancePackNames = a}) . _Default . _Coerce

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
dcpsNextToken :: Lens' DescribeConformancePackStatus (Maybe Text)
dcpsNextToken = lens _dcpsNextToken (\s a -> s {_dcpsNextToken = a})

-- | The maximum number of conformance packs status returned on each page.
dcpsLimit :: Lens' DescribeConformancePackStatus (Maybe Natural)
dcpsLimit = lens _dcpsLimit (\s a -> s {_dcpsLimit = a}) . mapping _Nat

instance AWSRequest DescribeConformancePackStatus where
  type
    Rs DescribeConformancePackStatus =
      DescribeConformancePackStatusResponse
  request = postJSON config
  response =
    receiveJSON
      ( \s h x ->
          DescribeConformancePackStatusResponse'
            <$> (x .?> "ConformancePackStatusDetails" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeConformancePackStatus

instance NFData DescribeConformancePackStatus

instance ToHeaders DescribeConformancePackStatus where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "StarlingDoveService.DescribeConformancePackStatus" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeConformancePackStatus where
  toJSON DescribeConformancePackStatus' {..} =
    object
      ( catMaybes
          [ ("ConformancePackNames" .=) <$> _dcpsConformancePackNames,
            ("NextToken" .=) <$> _dcpsNextToken,
            ("Limit" .=) <$> _dcpsLimit
          ]
      )

instance ToPath DescribeConformancePackStatus where
  toPath = const "/"

instance ToQuery DescribeConformancePackStatus where
  toQuery = const mempty

-- | /See:/ 'describeConformancePackStatusResponse' smart constructor.
data DescribeConformancePackStatusResponse = DescribeConformancePackStatusResponse'
  { _dcpsrsConformancePackStatusDetails ::
      !( Maybe
           [ConformancePackStatusDetail]
       ),
    _dcpsrsNextToken ::
      !(Maybe Text),
    _dcpsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeConformancePackStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpsrsConformancePackStatusDetails' - A list of @ConformancePackStatusDetail@ objects.
--
-- * 'dcpsrsNextToken' - The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- * 'dcpsrsResponseStatus' - -- | The response status code.
describeConformancePackStatusResponse ::
  -- | 'dcpsrsResponseStatus'
  Int ->
  DescribeConformancePackStatusResponse
describeConformancePackStatusResponse pResponseStatus_ =
  DescribeConformancePackStatusResponse'
    { _dcpsrsConformancePackStatusDetails =
        Nothing,
      _dcpsrsNextToken = Nothing,
      _dcpsrsResponseStatus = pResponseStatus_
    }

-- | A list of @ConformancePackStatusDetail@ objects.
dcpsrsConformancePackStatusDetails :: Lens' DescribeConformancePackStatusResponse [ConformancePackStatusDetail]
dcpsrsConformancePackStatusDetails = lens _dcpsrsConformancePackStatusDetails (\s a -> s {_dcpsrsConformancePackStatusDetails = a}) . _Default . _Coerce

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
dcpsrsNextToken :: Lens' DescribeConformancePackStatusResponse (Maybe Text)
dcpsrsNextToken = lens _dcpsrsNextToken (\s a -> s {_dcpsrsNextToken = a})

-- | -- | The response status code.
dcpsrsResponseStatus :: Lens' DescribeConformancePackStatusResponse Int
dcpsrsResponseStatus = lens _dcpsrsResponseStatus (\s a -> s {_dcpsrsResponseStatus = a})

instance NFData DescribeConformancePackStatusResponse
