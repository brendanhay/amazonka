{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DescribeSnapshots
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about the directory snapshots that belong to this account.
--
--
-- This operation supports pagination with the use of the /NextToken/ request and response parameters. If more results are available, the /DescribeSnapshots.NextToken/ member contains a token that you pass in the next call to 'DescribeSnapshots' to retrieve the next set of items.
--
-- You can also specify a maximum number of return results with the /Limit/ parameter.
--
module Network.AWS.DirectoryService.DescribeSnapshots
    (
    -- * Creating a Request
      describeSnapshots
    , DescribeSnapshots
    -- * Request Lenses
    , dsDirectoryId
    , dsNextToken
    , dsSnapshotIds
    , dsLimit

    -- * Destructuring the Response
    , describeSnapshotsResponse
    , DescribeSnapshotsResponse
    -- * Response Lenses
    , dssrsNextToken
    , dssrsSnapshots
    , dssrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the inputs for the 'DescribeSnapshots' operation.
--
--
--
-- /See:/ 'describeSnapshots' smart constructor.
data DescribeSnapshots = DescribeSnapshots'
  { _dsDirectoryId :: !(Maybe Text)
  , _dsNextToken   :: !(Maybe Text)
  , _dsSnapshotIds :: !(Maybe [Text])
  , _dsLimit       :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSnapshots' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsDirectoryId' - The identifier of the directory for which to retrieve snapshot information.
--
-- * 'dsNextToken' - The /DescribeSnapshotsResult.NextToken/ value from a previous call to 'DescribeSnapshots' . Pass null if this is the first call.
--
-- * 'dsSnapshotIds' - A list of identifiers of the snapshots to obtain the information for. If this member is null or empty, all snapshots are returned using the /Limit/ and /NextToken/ members.
--
-- * 'dsLimit' - The maximum number of objects to return.
describeSnapshots
    :: DescribeSnapshots
describeSnapshots =
  DescribeSnapshots'
    { _dsDirectoryId = Nothing
    , _dsNextToken = Nothing
    , _dsSnapshotIds = Nothing
    , _dsLimit = Nothing
    }


-- | The identifier of the directory for which to retrieve snapshot information.
dsDirectoryId :: Lens' DescribeSnapshots (Maybe Text)
dsDirectoryId = lens _dsDirectoryId (\ s a -> s{_dsDirectoryId = a})

-- | The /DescribeSnapshotsResult.NextToken/ value from a previous call to 'DescribeSnapshots' . Pass null if this is the first call.
dsNextToken :: Lens' DescribeSnapshots (Maybe Text)
dsNextToken = lens _dsNextToken (\ s a -> s{_dsNextToken = a})

-- | A list of identifiers of the snapshots to obtain the information for. If this member is null or empty, all snapshots are returned using the /Limit/ and /NextToken/ members.
dsSnapshotIds :: Lens' DescribeSnapshots [Text]
dsSnapshotIds = lens _dsSnapshotIds (\ s a -> s{_dsSnapshotIds = a}) . _Default . _Coerce

-- | The maximum number of objects to return.
dsLimit :: Lens' DescribeSnapshots (Maybe Natural)
dsLimit = lens _dsLimit (\ s a -> s{_dsLimit = a}) . mapping _Nat

instance AWSRequest DescribeSnapshots where
        type Rs DescribeSnapshots = DescribeSnapshotsResponse
        request = postJSON directoryService
        response
          = receiveJSON
              (\ s h x ->
                 DescribeSnapshotsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Snapshots" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeSnapshots where

instance NFData DescribeSnapshots where

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
              (catMaybes
                 [("DirectoryId" .=) <$> _dsDirectoryId,
                  ("NextToken" .=) <$> _dsNextToken,
                  ("SnapshotIds" .=) <$> _dsSnapshotIds,
                  ("Limit" .=) <$> _dsLimit])

instance ToPath DescribeSnapshots where
        toPath = const "/"

instance ToQuery DescribeSnapshots where
        toQuery = const mempty

-- | Contains the results of the 'DescribeSnapshots' operation.
--
--
--
-- /See:/ 'describeSnapshotsResponse' smart constructor.
data DescribeSnapshotsResponse = DescribeSnapshotsResponse'
  { _dssrsNextToken      :: !(Maybe Text)
  , _dssrsSnapshots      :: !(Maybe [Snapshot])
  , _dssrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSnapshotsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssrsNextToken' - If not null, more results are available. Pass this value in the /NextToken/ member of a subsequent call to 'DescribeSnapshots' .
--
-- * 'dssrsSnapshots' - The list of 'Snapshot' objects that were retrieved. It is possible that this list contains less than the number of items specified in the /Limit/ member of the request. This occurs if there are less than the requested number of items left to retrieve, or if the limitations of the operation have been exceeded.
--
-- * 'dssrsResponseStatus' - -- | The response status code.
describeSnapshotsResponse
    :: Int -- ^ 'dssrsResponseStatus'
    -> DescribeSnapshotsResponse
describeSnapshotsResponse pResponseStatus_ =
  DescribeSnapshotsResponse'
    { _dssrsNextToken = Nothing
    , _dssrsSnapshots = Nothing
    , _dssrsResponseStatus = pResponseStatus_
    }


-- | If not null, more results are available. Pass this value in the /NextToken/ member of a subsequent call to 'DescribeSnapshots' .
dssrsNextToken :: Lens' DescribeSnapshotsResponse (Maybe Text)
dssrsNextToken = lens _dssrsNextToken (\ s a -> s{_dssrsNextToken = a})

-- | The list of 'Snapshot' objects that were retrieved. It is possible that this list contains less than the number of items specified in the /Limit/ member of the request. This occurs if there are less than the requested number of items left to retrieve, or if the limitations of the operation have been exceeded.
dssrsSnapshots :: Lens' DescribeSnapshotsResponse [Snapshot]
dssrsSnapshots = lens _dssrsSnapshots (\ s a -> s{_dssrsSnapshots = a}) . _Default . _Coerce

-- | -- | The response status code.
dssrsResponseStatus :: Lens' DescribeSnapshotsResponse Int
dssrsResponseStatus = lens _dssrsResponseStatus (\ s a -> s{_dssrsResponseStatus = a})

instance NFData DescribeSnapshotsResponse where
