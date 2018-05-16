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
-- Module      : Network.AWS.OpsWorksCM.DescribeBackups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes backups. The results are ordered by time, with newest backups first. If you do not specify a BackupId or ServerName, the command returns all backups.
--
--
-- This operation is synchronous.
--
-- A @ResourceNotFoundException@ is thrown when the backup does not exist. A @ValidationException@ is raised when parameters of the request are not valid.
--
module Network.AWS.OpsWorksCM.DescribeBackups
    (
    -- * Creating a Request
      describeBackups
    , DescribeBackups
    -- * Request Lenses
    , dServerName
    , dBackupId
    , dNextToken
    , dMaxResults

    -- * Destructuring the Response
    , describeBackupsResponse
    , DescribeBackupsResponse
    -- * Response Lenses
    , drsBackups
    , drsNextToken
    , drsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorksCM.Types
import Network.AWS.OpsWorksCM.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeBackups' smart constructor.
data DescribeBackups = DescribeBackups'
  { _dServerName :: !(Maybe Text)
  , _dBackupId   :: !(Maybe Text)
  , _dNextToken  :: !(Maybe Text)
  , _dMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeBackups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dServerName' - Returns backups for the server with the specified ServerName.
--
-- * 'dBackupId' - Describes a single backup.
--
-- * 'dNextToken' - NextToken is a string that is returned in some command responses. It indicates that not all entries have been returned, and that you must run at least one more request to get remaining items. To get remaining results, call @DescribeBackups@ again, and assign the token from the previous results as the value of the @nextToken@ parameter. If there are no more results, the response object's @nextToken@ parameter value is @null@ . Setting a @nextToken@ value that was not returned in your previous results causes an @InvalidNextTokenException@ to occur.
--
-- * 'dMaxResults' - To receive a paginated response, use this parameter to specify the maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
describeBackups
    :: DescribeBackups
describeBackups =
  DescribeBackups'
    { _dServerName = Nothing
    , _dBackupId = Nothing
    , _dNextToken = Nothing
    , _dMaxResults = Nothing
    }


-- | Returns backups for the server with the specified ServerName.
dServerName :: Lens' DescribeBackups (Maybe Text)
dServerName = lens _dServerName (\ s a -> s{_dServerName = a})

-- | Describes a single backup.
dBackupId :: Lens' DescribeBackups (Maybe Text)
dBackupId = lens _dBackupId (\ s a -> s{_dBackupId = a})

-- | NextToken is a string that is returned in some command responses. It indicates that not all entries have been returned, and that you must run at least one more request to get remaining items. To get remaining results, call @DescribeBackups@ again, and assign the token from the previous results as the value of the @nextToken@ parameter. If there are no more results, the response object's @nextToken@ parameter value is @null@ . Setting a @nextToken@ value that was not returned in your previous results causes an @InvalidNextTokenException@ to occur.
dNextToken :: Lens' DescribeBackups (Maybe Text)
dNextToken = lens _dNextToken (\ s a -> s{_dNextToken = a})

-- | To receive a paginated response, use this parameter to specify the maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
dMaxResults :: Lens' DescribeBackups (Maybe Natural)
dMaxResults = lens _dMaxResults (\ s a -> s{_dMaxResults = a}) . mapping _Nat

instance AWSRequest DescribeBackups where
        type Rs DescribeBackups = DescribeBackupsResponse
        request = postJSON opsWorksCM
        response
          = receiveJSON
              (\ s h x ->
                 DescribeBackupsResponse' <$>
                   (x .?> "Backups" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeBackups where

instance NFData DescribeBackups where

instance ToHeaders DescribeBackups where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorksCM_V2016_11_01.DescribeBackups" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeBackups where
        toJSON DescribeBackups'{..}
          = object
              (catMaybes
                 [("ServerName" .=) <$> _dServerName,
                  ("BackupId" .=) <$> _dBackupId,
                  ("NextToken" .=) <$> _dNextToken,
                  ("MaxResults" .=) <$> _dMaxResults])

instance ToPath DescribeBackups where
        toPath = const "/"

instance ToQuery DescribeBackups where
        toQuery = const mempty

-- | /See:/ 'describeBackupsResponse' smart constructor.
data DescribeBackupsResponse = DescribeBackupsResponse'
  { _drsBackups        :: !(Maybe [Backup])
  , _drsNextToken      :: !(Maybe Text)
  , _drsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeBackupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsBackups' - Contains the response to a @DescribeBackups@ request.
--
-- * 'drsNextToken' - NextToken is a string that is returned in some command responses. It indicates that not all entries have been returned, and that you must run at least one more request to get remaining items. To get remaining results, call @DescribeBackups@ again, and assign the token from the previous results as the value of the @nextToken@ parameter. If there are no more results, the response object's @nextToken@ parameter value is @null@ . Setting a @nextToken@ value that was not returned in your previous results causes an @InvalidNextTokenException@ to occur.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeBackupsResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeBackupsResponse
describeBackupsResponse pResponseStatus_ =
  DescribeBackupsResponse'
    { _drsBackups = Nothing
    , _drsNextToken = Nothing
    , _drsResponseStatus = pResponseStatus_
    }


-- | Contains the response to a @DescribeBackups@ request.
drsBackups :: Lens' DescribeBackupsResponse [Backup]
drsBackups = lens _drsBackups (\ s a -> s{_drsBackups = a}) . _Default . _Coerce

-- | NextToken is a string that is returned in some command responses. It indicates that not all entries have been returned, and that you must run at least one more request to get remaining items. To get remaining results, call @DescribeBackups@ again, and assign the token from the previous results as the value of the @nextToken@ parameter. If there are no more results, the response object's @nextToken@ parameter value is @null@ . Setting a @nextToken@ value that was not returned in your previous results causes an @InvalidNextTokenException@ to occur.
drsNextToken :: Lens' DescribeBackupsResponse (Maybe Text)
drsNextToken = lens _drsNextToken (\ s a -> s{_drsNextToken = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeBackupsResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeBackupsResponse where
