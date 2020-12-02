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
-- Module      : Network.AWS.EC2.DescribeFastSnapshotRestores
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the state of fast snapshot restores for your snapshots.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeFastSnapshotRestores
  ( -- * Creating a Request
    describeFastSnapshotRestores,
    DescribeFastSnapshotRestores,

    -- * Request Lenses
    dfsrFilters,
    dfsrNextToken,
    dfsrDryRun,
    dfsrMaxResults,

    -- * Destructuring the Response
    describeFastSnapshotRestoresResponse,
    DescribeFastSnapshotRestoresResponse,

    -- * Response Lenses
    dfsrsrsFastSnapshotRestores,
    dfsrsrsNextToken,
    dfsrsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeFastSnapshotRestores' smart constructor.
data DescribeFastSnapshotRestores = DescribeFastSnapshotRestores'
  { _dfsrFilters ::
      !(Maybe [Filter]),
    _dfsrNextToken :: !(Maybe Text),
    _dfsrDryRun :: !(Maybe Bool),
    _dfsrMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeFastSnapshotRestores' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfsrFilters' - The filters. The possible values are:     * @availability-zone@ : The Availability Zone of the snapshot.     * @owner-id@ : The ID of the AWS account that enabled fast snapshot restore on the snapshot.     * @snapshot-id@ : The ID of the snapshot.     * @state@ : The state of fast snapshot restores for the snapshot (@enabling@ | @optimizing@ | @enabled@ | @disabling@ | @disabled@ ).
--
-- * 'dfsrNextToken' - The token for the next page of results.
--
-- * 'dfsrDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dfsrMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeFastSnapshotRestores ::
  DescribeFastSnapshotRestores
describeFastSnapshotRestores =
  DescribeFastSnapshotRestores'
    { _dfsrFilters = Nothing,
      _dfsrNextToken = Nothing,
      _dfsrDryRun = Nothing,
      _dfsrMaxResults = Nothing
    }

-- | The filters. The possible values are:     * @availability-zone@ : The Availability Zone of the snapshot.     * @owner-id@ : The ID of the AWS account that enabled fast snapshot restore on the snapshot.     * @snapshot-id@ : The ID of the snapshot.     * @state@ : The state of fast snapshot restores for the snapshot (@enabling@ | @optimizing@ | @enabled@ | @disabling@ | @disabled@ ).
dfsrFilters :: Lens' DescribeFastSnapshotRestores [Filter]
dfsrFilters = lens _dfsrFilters (\s a -> s {_dfsrFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
dfsrNextToken :: Lens' DescribeFastSnapshotRestores (Maybe Text)
dfsrNextToken = lens _dfsrNextToken (\s a -> s {_dfsrNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dfsrDryRun :: Lens' DescribeFastSnapshotRestores (Maybe Bool)
dfsrDryRun = lens _dfsrDryRun (\s a -> s {_dfsrDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dfsrMaxResults :: Lens' DescribeFastSnapshotRestores (Maybe Natural)
dfsrMaxResults = lens _dfsrMaxResults (\s a -> s {_dfsrMaxResults = a}) . mapping _Nat

instance AWSPager DescribeFastSnapshotRestores where
  page rq rs
    | stop (rs ^. dfsrsrsNextToken) = Nothing
    | stop (rs ^. dfsrsrsFastSnapshotRestores) = Nothing
    | otherwise = Just $ rq & dfsrNextToken .~ rs ^. dfsrsrsNextToken

instance AWSRequest DescribeFastSnapshotRestores where
  type
    Rs DescribeFastSnapshotRestores =
      DescribeFastSnapshotRestoresResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeFastSnapshotRestoresResponse'
            <$> ( x .@? "fastSnapshotRestoreSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (x .@? "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeFastSnapshotRestores

instance NFData DescribeFastSnapshotRestores

instance ToHeaders DescribeFastSnapshotRestores where
  toHeaders = const mempty

instance ToPath DescribeFastSnapshotRestores where
  toPath = const "/"

instance ToQuery DescribeFastSnapshotRestores where
  toQuery DescribeFastSnapshotRestores' {..} =
    mconcat
      [ "Action" =: ("DescribeFastSnapshotRestores" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _dfsrFilters),
        "NextToken" =: _dfsrNextToken,
        "DryRun" =: _dfsrDryRun,
        "MaxResults" =: _dfsrMaxResults
      ]

-- | /See:/ 'describeFastSnapshotRestoresResponse' smart constructor.
data DescribeFastSnapshotRestoresResponse = DescribeFastSnapshotRestoresResponse'
  { _dfsrsrsFastSnapshotRestores ::
      !( Maybe
           [DescribeFastSnapshotRestoreSuccessItem]
       ),
    _dfsrsrsNextToken ::
      !(Maybe Text),
    _dfsrsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeFastSnapshotRestoresResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfsrsrsFastSnapshotRestores' - Information about the state of fast snapshot restores.
--
-- * 'dfsrsrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dfsrsrsResponseStatus' - -- | The response status code.
describeFastSnapshotRestoresResponse ::
  -- | 'dfsrsrsResponseStatus'
  Int ->
  DescribeFastSnapshotRestoresResponse
describeFastSnapshotRestoresResponse pResponseStatus_ =
  DescribeFastSnapshotRestoresResponse'
    { _dfsrsrsFastSnapshotRestores =
        Nothing,
      _dfsrsrsNextToken = Nothing,
      _dfsrsrsResponseStatus = pResponseStatus_
    }

-- | Information about the state of fast snapshot restores.
dfsrsrsFastSnapshotRestores :: Lens' DescribeFastSnapshotRestoresResponse [DescribeFastSnapshotRestoreSuccessItem]
dfsrsrsFastSnapshotRestores = lens _dfsrsrsFastSnapshotRestores (\s a -> s {_dfsrsrsFastSnapshotRestores = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dfsrsrsNextToken :: Lens' DescribeFastSnapshotRestoresResponse (Maybe Text)
dfsrsrsNextToken = lens _dfsrsrsNextToken (\s a -> s {_dfsrsrsNextToken = a})

-- | -- | The response status code.
dfsrsrsResponseStatus :: Lens' DescribeFastSnapshotRestoresResponse Int
dfsrsrsResponseStatus = lens _dfsrsrsResponseStatus (\s a -> s {_dfsrsrsResponseStatus = a})

instance NFData DescribeFastSnapshotRestoresResponse
