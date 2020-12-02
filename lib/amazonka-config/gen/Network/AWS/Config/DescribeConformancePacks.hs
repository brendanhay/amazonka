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
-- Module      : Network.AWS.Config.DescribeConformancePacks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of one or more conformance packs.
module Network.AWS.Config.DescribeConformancePacks
  ( -- * Creating a Request
    describeConformancePacks,
    DescribeConformancePacks,

    -- * Request Lenses
    dcpConformancePackNames,
    dcpNextToken,
    dcpLimit,

    -- * Destructuring the Response
    describeConformancePacksResponse,
    DescribeConformancePacksResponse,

    -- * Response Lenses
    dcprsNextToken,
    dcprsConformancePackDetails,
    dcprsResponseStatus,
  )
where

import Network.AWS.Config.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeConformancePacks' smart constructor.
data DescribeConformancePacks = DescribeConformancePacks'
  { _dcpConformancePackNames ::
      !(Maybe [Text]),
    _dcpNextToken :: !(Maybe Text),
    _dcpLimit :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeConformancePacks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpConformancePackNames' - Comma-separated list of conformance pack names for which you want details. If you do not specify any names, AWS Config returns details for all your conformance packs.
--
-- * 'dcpNextToken' - The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- * 'dcpLimit' - The maximum number of conformance packs returned on each page.
describeConformancePacks ::
  DescribeConformancePacks
describeConformancePacks =
  DescribeConformancePacks'
    { _dcpConformancePackNames = Nothing,
      _dcpNextToken = Nothing,
      _dcpLimit = Nothing
    }

-- | Comma-separated list of conformance pack names for which you want details. If you do not specify any names, AWS Config returns details for all your conformance packs.
dcpConformancePackNames :: Lens' DescribeConformancePacks [Text]
dcpConformancePackNames = lens _dcpConformancePackNames (\s a -> s {_dcpConformancePackNames = a}) . _Default . _Coerce

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
dcpNextToken :: Lens' DescribeConformancePacks (Maybe Text)
dcpNextToken = lens _dcpNextToken (\s a -> s {_dcpNextToken = a})

-- | The maximum number of conformance packs returned on each page.
dcpLimit :: Lens' DescribeConformancePacks (Maybe Natural)
dcpLimit = lens _dcpLimit (\s a -> s {_dcpLimit = a}) . mapping _Nat

instance AWSRequest DescribeConformancePacks where
  type Rs DescribeConformancePacks = DescribeConformancePacksResponse
  request = postJSON config
  response =
    receiveJSON
      ( \s h x ->
          DescribeConformancePacksResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "ConformancePackDetails" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeConformancePacks

instance NFData DescribeConformancePacks

instance ToHeaders DescribeConformancePacks where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("StarlingDoveService.DescribeConformancePacks" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeConformancePacks where
  toJSON DescribeConformancePacks' {..} =
    object
      ( catMaybes
          [ ("ConformancePackNames" .=) <$> _dcpConformancePackNames,
            ("NextToken" .=) <$> _dcpNextToken,
            ("Limit" .=) <$> _dcpLimit
          ]
      )

instance ToPath DescribeConformancePacks where
  toPath = const "/"

instance ToQuery DescribeConformancePacks where
  toQuery = const mempty

-- | /See:/ 'describeConformancePacksResponse' smart constructor.
data DescribeConformancePacksResponse = DescribeConformancePacksResponse'
  { _dcprsNextToken ::
      !(Maybe Text),
    _dcprsConformancePackDetails ::
      !( Maybe
           [ConformancePackDetail]
       ),
    _dcprsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeConformancePacksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcprsNextToken' - The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- * 'dcprsConformancePackDetails' - Returns a list of @ConformancePackDetail@ objects.
--
-- * 'dcprsResponseStatus' - -- | The response status code.
describeConformancePacksResponse ::
  -- | 'dcprsResponseStatus'
  Int ->
  DescribeConformancePacksResponse
describeConformancePacksResponse pResponseStatus_ =
  DescribeConformancePacksResponse'
    { _dcprsNextToken = Nothing,
      _dcprsConformancePackDetails = Nothing,
      _dcprsResponseStatus = pResponseStatus_
    }

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
dcprsNextToken :: Lens' DescribeConformancePacksResponse (Maybe Text)
dcprsNextToken = lens _dcprsNextToken (\s a -> s {_dcprsNextToken = a})

-- | Returns a list of @ConformancePackDetail@ objects.
dcprsConformancePackDetails :: Lens' DescribeConformancePacksResponse [ConformancePackDetail]
dcprsConformancePackDetails = lens _dcprsConformancePackDetails (\s a -> s {_dcprsConformancePackDetails = a}) . _Default . _Coerce

-- | -- | The response status code.
dcprsResponseStatus :: Lens' DescribeConformancePacksResponse Int
dcprsResponseStatus = lens _dcprsResponseStatus (\s a -> s {_dcprsResponseStatus = a})

instance NFData DescribeConformancePacksResponse
