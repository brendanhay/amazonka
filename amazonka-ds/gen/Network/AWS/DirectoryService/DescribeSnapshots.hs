{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DescribeSnapshots
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
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
module Network.AWS.DirectoryService.DescribeSnapshots
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
    , desNextToken
    , desSnapshots
    , desStatus
    ) where

import           Network.AWS.DirectoryService.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the DescribeSnapshots operation.
--
-- /See:/ 'describeSnapshots' smart constructor.
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
data DescribeSnapshots = DescribeSnapshots'
    { _dsDirectoryId :: !(Maybe Text)
    , _dsNextToken   :: !(Maybe Text)
    , _dsSnapshotIds :: !(Maybe [Text])
    , _dsLimit       :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSnapshots' smart constructor.
describeSnapshots :: DescribeSnapshots
describeSnapshots =
    DescribeSnapshots'
    { _dsDirectoryId = Nothing
    , _dsNextToken = Nothing
    , _dsSnapshotIds = Nothing
    , _dsLimit = Nothing
    }

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
dsSnapshotIds = lens _dsSnapshotIds (\ s a -> s{_dsSnapshotIds = a}) . _Default;

-- | The maximum number of objects to return.
dsLimit :: Lens' DescribeSnapshots (Maybe Natural)
dsLimit = lens _dsLimit (\ s a -> s{_dsLimit = a}) . mapping _Nat;

instance AWSRequest DescribeSnapshots where
        type Sv DescribeSnapshots = DirectoryService
        type Rs DescribeSnapshots = DescribeSnapshotsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeSnapshotsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Snapshots" .!@ mempty)
                     <*> (pure (fromEnum s)))

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

-- | Contains the results of the DescribeSnapshots operation.
--
-- /See:/ 'describeSnapshotsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desNextToken'
--
-- * 'desSnapshots'
--
-- * 'desStatus'
data DescribeSnapshotsResponse = DescribeSnapshotsResponse'
    { _desNextToken :: !(Maybe Text)
    , _desSnapshots :: !(Maybe [Snapshot])
    , _desStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSnapshotsResponse' smart constructor.
describeSnapshotsResponse :: Int -> DescribeSnapshotsResponse
describeSnapshotsResponse pStatus =
    DescribeSnapshotsResponse'
    { _desNextToken = Nothing
    , _desSnapshots = Nothing
    , _desStatus = pStatus
    }

-- | If not null, more results are available. Pass this value in the
-- /NextToken/ member of a subsequent call to DescribeSnapshots.
desNextToken :: Lens' DescribeSnapshotsResponse (Maybe Text)
desNextToken = lens _desNextToken (\ s a -> s{_desNextToken = a});

-- | The list of Snapshot objects that were retrieved.
--
-- It is possible that this list contains less than the number of items
-- specified in the /Limit/ member of the request. This occurs if there are
-- less than the requested number of items left to retrieve, or if the
-- limitations of the operation have been exceeded.
desSnapshots :: Lens' DescribeSnapshotsResponse [Snapshot]
desSnapshots = lens _desSnapshots (\ s a -> s{_desSnapshots = a}) . _Default;

-- | FIXME: Undocumented member.
desStatus :: Lens' DescribeSnapshotsResponse Int
desStatus = lens _desStatus (\ s a -> s{_desStatus = a});
