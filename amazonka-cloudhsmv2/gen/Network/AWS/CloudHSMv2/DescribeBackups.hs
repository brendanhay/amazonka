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
-- Module      : Network.AWS.CloudHSMv2.DescribeBackups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about backups of AWS CloudHSM clusters.
--
--
-- This is a paginated operation, which means that each response might contain only a subset of all the backups. When the response contains only a subset of backups, it includes a @NextToken@ value. Use this value in a subsequent @DescribeBackups@ request to get more backups. When you receive a response with no @NextToken@ (or an empty or null value), that means there are no more backups to get.
--
--
-- This operation returns paginated results.
module Network.AWS.CloudHSMv2.DescribeBackups
    (
    -- * Creating a Request
      describeBackups
    , DescribeBackups
    -- * Request Lenses
    , dbSortAscending
    , dbFilters
    , dbNextToken
    , dbMaxResults

    -- * Destructuring the Response
    , describeBackupsResponse
    , DescribeBackupsResponse
    -- * Response Lenses
    , dbsrsBackups
    , dbsrsNextToken
    , dbsrsResponseStatus
    ) where

import Network.AWS.CloudHSMv2.Types
import Network.AWS.CloudHSMv2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeBackups' smart constructor.
data DescribeBackups = DescribeBackups'
  { _dbSortAscending :: !(Maybe Bool)
  , _dbFilters       :: !(Maybe (Map Text [Text]))
  , _dbNextToken     :: !(Maybe Text)
  , _dbMaxResults    :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeBackups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbSortAscending' - Undocumented member.
--
-- * 'dbFilters' - One or more filters to limit the items returned in the response. Use the @backupIds@ filter to return only the specified backups. Specify backups by their backup identifier (ID). Use the @sourceBackupIds@ filter to return only the backups created from a source backup. The @sourceBackupID@ of a source backup is returned by the 'CopyBackupToRegion' operation. Use the @clusterIds@ filter to return only the backups for the specified clusters. Specify clusters by their cluster identifier (ID). Use the @states@ filter to return only backups that match the specified state.
--
-- * 'dbNextToken' - The @NextToken@ value that you received in the previous response. Use this value to get more backups.
--
-- * 'dbMaxResults' - The maximum number of backups to return in the response. When there are more backups than the number you specify, the response contains a @NextToken@ value.
describeBackups
    :: DescribeBackups
describeBackups =
  DescribeBackups'
    { _dbSortAscending = Nothing
    , _dbFilters = Nothing
    , _dbNextToken = Nothing
    , _dbMaxResults = Nothing
    }


-- | Undocumented member.
dbSortAscending :: Lens' DescribeBackups (Maybe Bool)
dbSortAscending = lens _dbSortAscending (\ s a -> s{_dbSortAscending = a})

-- | One or more filters to limit the items returned in the response. Use the @backupIds@ filter to return only the specified backups. Specify backups by their backup identifier (ID). Use the @sourceBackupIds@ filter to return only the backups created from a source backup. The @sourceBackupID@ of a source backup is returned by the 'CopyBackupToRegion' operation. Use the @clusterIds@ filter to return only the backups for the specified clusters. Specify clusters by their cluster identifier (ID). Use the @states@ filter to return only backups that match the specified state.
dbFilters :: Lens' DescribeBackups (HashMap Text [Text])
dbFilters = lens _dbFilters (\ s a -> s{_dbFilters = a}) . _Default . _Map

-- | The @NextToken@ value that you received in the previous response. Use this value to get more backups.
dbNextToken :: Lens' DescribeBackups (Maybe Text)
dbNextToken = lens _dbNextToken (\ s a -> s{_dbNextToken = a})

-- | The maximum number of backups to return in the response. When there are more backups than the number you specify, the response contains a @NextToken@ value.
dbMaxResults :: Lens' DescribeBackups (Maybe Natural)
dbMaxResults = lens _dbMaxResults (\ s a -> s{_dbMaxResults = a}) . mapping _Nat

instance AWSPager DescribeBackups where
        page rq rs
          | stop (rs ^. dbsrsNextToken) = Nothing
          | stop (rs ^. dbsrsBackups) = Nothing
          | otherwise =
            Just $ rq & dbNextToken .~ rs ^. dbsrsNextToken

instance AWSRequest DescribeBackups where
        type Rs DescribeBackups = DescribeBackupsResponse
        request = postJSON cloudHSMv2
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
                    ("BaldrApiService.DescribeBackups" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeBackups where
        toJSON DescribeBackups'{..}
          = object
              (catMaybes
                 [("SortAscending" .=) <$> _dbSortAscending,
                  ("Filters" .=) <$> _dbFilters,
                  ("NextToken" .=) <$> _dbNextToken,
                  ("MaxResults" .=) <$> _dbMaxResults])

instance ToPath DescribeBackups where
        toPath = const "/"

instance ToQuery DescribeBackups where
        toQuery = const mempty

-- | /See:/ 'describeBackupsResponse' smart constructor.
data DescribeBackupsResponse = DescribeBackupsResponse'
  { _dbsrsBackups        :: !(Maybe [Backup])
  , _dbsrsNextToken      :: !(Maybe Text)
  , _dbsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeBackupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbsrsBackups' - A list of backups.
--
-- * 'dbsrsNextToken' - An opaque string that indicates that the response contains only a subset of backups. Use this value in a subsequent @DescribeBackups@ request to get more backups.
--
-- * 'dbsrsResponseStatus' - -- | The response status code.
describeBackupsResponse
    :: Int -- ^ 'dbsrsResponseStatus'
    -> DescribeBackupsResponse
describeBackupsResponse pResponseStatus_ =
  DescribeBackupsResponse'
    { _dbsrsBackups = Nothing
    , _dbsrsNextToken = Nothing
    , _dbsrsResponseStatus = pResponseStatus_
    }


-- | A list of backups.
dbsrsBackups :: Lens' DescribeBackupsResponse [Backup]
dbsrsBackups = lens _dbsrsBackups (\ s a -> s{_dbsrsBackups = a}) . _Default . _Coerce

-- | An opaque string that indicates that the response contains only a subset of backups. Use this value in a subsequent @DescribeBackups@ request to get more backups.
dbsrsNextToken :: Lens' DescribeBackupsResponse (Maybe Text)
dbsrsNextToken = lens _dbsrsNextToken (\ s a -> s{_dbsrsNextToken = a})

-- | -- | The response status code.
dbsrsResponseStatus :: Lens' DescribeBackupsResponse Int
dbsrsResponseStatus = lens _dbsrsResponseStatus (\ s a -> s{_dbsrsResponseStatus = a})

instance NFData DescribeBackupsResponse where
