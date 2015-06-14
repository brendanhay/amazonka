{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Directory Service.DescribeSnapshots
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Obtains information about the directory snapshots that belong to this
-- account.
--
-- This operation supports pagination with the use of the /NextToken/
-- request and response parameters. If more results are available, the
-- /DescribeSnapshots.NextToken/ member contains a token that you pass in
-- the next call to DescribeSnapshots to retrieve the next set of items.
--
-- You can also specify a maximum number of return results with the /Limit/
-- parameter.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_DescribeSnapshots.html>
module Network.AWS.Directory Service.DescribeSnapshots
    (
    -- * Request
      DescribeSnapshots
    -- ** Request constructor
    , describeSnapshots
    -- ** Request lenses
    , dsDirectoryId
    , dsNextToken
    , dsSnapshotIds
    , dsLimit

    -- * Response
    , DescribeSnapshotsResponse
    -- ** Response constructor
    , describeSnapshotsResponse
    -- ** Response lenses
    , dsrNextToken
    , dsrSnapshots
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Directory Service.Types

-- | /See:/ 'describeSnapshots' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsDirectoryId'
--
-- * 'dsNextToken'
--
-- * 'dsSnapshotIds'
--
-- * 'dsLimit'
data DescribeSnapshots = DescribeSnapshots'{_dsDirectoryId :: Maybe Text, _dsNextToken :: Maybe Text, _dsSnapshotIds :: [Text], _dsLimit :: Maybe Nat} deriving (Eq, Read, Show)

-- | 'DescribeSnapshots' smart constructor.
describeSnapshots :: DescribeSnapshots
describeSnapshots = DescribeSnapshots'{_dsDirectoryId = Nothing, _dsNextToken = Nothing, _dsSnapshotIds = mempty, _dsLimit = Nothing};

-- | The identifier of the directory to retrieve snapshot information for.
dsDirectoryId :: Lens' DescribeSnapshots (Maybe Text)
dsDirectoryId = lens _dsDirectoryId (\ s a -> s{_dsDirectoryId = a});

-- | The /DescribeSnapshotsResult.NextToken/ value from a previous call to
-- DescribeSnapshots. Pass null if this is the first call.
dsNextToken :: Lens' DescribeSnapshots (Maybe Text)
dsNextToken = lens _dsNextToken (\ s a -> s{_dsNextToken = a});

-- | A list of identifiers of the snapshots to obtain the information for. If
-- this member is null or empty, all snapshots are returned using the
-- /Limit/ and /NextToken/ members.
dsSnapshotIds :: Lens' DescribeSnapshots [Text]
dsSnapshotIds = lens _dsSnapshotIds (\ s a -> s{_dsSnapshotIds = a});

-- | The maximum number of objects to return.
dsLimit :: Lens' DescribeSnapshots (Maybe Natural)
dsLimit = lens _dsLimit (\ s a -> s{_dsLimit = a}) . mapping _Nat;

instance AWSRequest DescribeSnapshots where
        type Sv DescribeSnapshots = Directory Service
        type Rs DescribeSnapshots = DescribeSnapshotsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeSnapshotsResponse' <$>
                   x .?> "NextToken" <*> x .?> "Snapshots" .!@ mempty)

instance ToHeaders DescribeSnapshots where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.DescribeSnapshots" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeSnapshots where
        toJSON DescribeSnapshots'{..}
          = object
              ["DirectoryId" .= _dsDirectoryId,
               "NextToken" .= _dsNextToken,
               "SnapshotIds" .= _dsSnapshotIds, "Limit" .= _dsLimit]

instance ToPath DescribeSnapshots where
        toPath = const "/"

instance ToQuery DescribeSnapshots where
        toQuery = const mempty

-- | /See:/ 'describeSnapshotsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsrNextToken'
--
-- * 'dsrSnapshots'
data DescribeSnapshotsResponse = DescribeSnapshotsResponse'{_dsrNextToken :: Maybe Text, _dsrSnapshots :: [Snapshot]} deriving (Eq, Read, Show)

-- | 'DescribeSnapshotsResponse' smart constructor.
describeSnapshotsResponse :: DescribeSnapshotsResponse
describeSnapshotsResponse = DescribeSnapshotsResponse'{_dsrNextToken = Nothing, _dsrSnapshots = mempty};

-- | If not null, more results are available. Pass this value in the
-- /NextToken/ member of a subsequent call to DescribeSnapshots.
dsrNextToken :: Lens' DescribeSnapshotsResponse (Maybe Text)
dsrNextToken = lens _dsrNextToken (\ s a -> s{_dsrNextToken = a});

-- | The list of Snapshot objects that were retrieved.
--
-- It is possible that this list contains less than the number of items
-- specified in the /Limit/ member of the request. This occurs if there are
-- less than the requested number of items left to retrieve, or if the
-- limitations of the operation have been exceeded.
dsrSnapshots :: Lens' DescribeSnapshotsResponse [Snapshot]
dsrSnapshots = lens _dsrSnapshots (\ s a -> s{_dsrSnapshots = a});
