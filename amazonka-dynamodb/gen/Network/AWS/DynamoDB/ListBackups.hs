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
-- Module      : Network.AWS.DynamoDB.ListBackups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List backups associated with an AWS account. To list backups for a given table, specify @TableName@ . @ListBackups@ returns a paginated list of results with at most 1MB worth of items in a page. You can also specify a limit for the maximum number of entries to be returned in a page.
--
--
-- In the request, start time is inclusive but end time is exclusive. Note that these limits are for the time at which the original backup was requested.
--
-- You can call @ListBackups@ a maximum of 5 times per second.
--
--
-- This operation returns paginated results.
module Network.AWS.DynamoDB.ListBackups
    (
    -- * Creating a Request
      listBackups
    , ListBackups
    -- * Request Lenses
    , lbTimeRangeUpperBound
    , lbTimeRangeLowerBound
    , lbLimit
    , lbExclusiveStartBackupARN
    , lbTableName

    -- * Destructuring the Response
    , listBackupsResponse
    , ListBackupsResponse
    -- * Response Lenses
    , lbrsBackupSummaries
    , lbrsLastEvaluatedBackupARN
    , lbrsResponseStatus
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listBackups' smart constructor.
data ListBackups = ListBackups'
  { _lbTimeRangeUpperBound     :: !(Maybe POSIX)
  , _lbTimeRangeLowerBound     :: !(Maybe POSIX)
  , _lbLimit                   :: !(Maybe Nat)
  , _lbExclusiveStartBackupARN :: !(Maybe Text)
  , _lbTableName               :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBackups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbTimeRangeUpperBound' - Only backups created before this time are listed. @TimeRangeUpperBound@ is exclusive.
--
-- * 'lbTimeRangeLowerBound' - Only backups created after this time are listed. @TimeRangeLowerBound@ is inclusive.
--
-- * 'lbLimit' - Maximum number of backups to return at once.
--
-- * 'lbExclusiveStartBackupARN' - @LastEvaluatedBackupARN@ returned by the previous ListBackups call.
--
-- * 'lbTableName' - The backups from the table specified by TableName are listed.
listBackups
    :: ListBackups
listBackups =
  ListBackups'
    { _lbTimeRangeUpperBound = Nothing
    , _lbTimeRangeLowerBound = Nothing
    , _lbLimit = Nothing
    , _lbExclusiveStartBackupARN = Nothing
    , _lbTableName = Nothing
    }


-- | Only backups created before this time are listed. @TimeRangeUpperBound@ is exclusive.
lbTimeRangeUpperBound :: Lens' ListBackups (Maybe UTCTime)
lbTimeRangeUpperBound = lens _lbTimeRangeUpperBound (\ s a -> s{_lbTimeRangeUpperBound = a}) . mapping _Time

-- | Only backups created after this time are listed. @TimeRangeLowerBound@ is inclusive.
lbTimeRangeLowerBound :: Lens' ListBackups (Maybe UTCTime)
lbTimeRangeLowerBound = lens _lbTimeRangeLowerBound (\ s a -> s{_lbTimeRangeLowerBound = a}) . mapping _Time

-- | Maximum number of backups to return at once.
lbLimit :: Lens' ListBackups (Maybe Natural)
lbLimit = lens _lbLimit (\ s a -> s{_lbLimit = a}) . mapping _Nat

-- | @LastEvaluatedBackupARN@ returned by the previous ListBackups call.
lbExclusiveStartBackupARN :: Lens' ListBackups (Maybe Text)
lbExclusiveStartBackupARN = lens _lbExclusiveStartBackupARN (\ s a -> s{_lbExclusiveStartBackupARN = a})

-- | The backups from the table specified by TableName are listed.
lbTableName :: Lens' ListBackups (Maybe Text)
lbTableName = lens _lbTableName (\ s a -> s{_lbTableName = a})

instance AWSPager ListBackups where
        page rq rs
          | stop (rs ^. lbrsLastEvaluatedBackupARN) = Nothing
          | stop (rs ^. lbrsBackupSummaries) = Nothing
          | otherwise =
            Just $ rq &
              lbExclusiveStartBackupARN .~
                rs ^. lbrsLastEvaluatedBackupARN

instance AWSRequest ListBackups where
        type Rs ListBackups = ListBackupsResponse
        request = postJSON dynamoDB
        response
          = receiveJSON
              (\ s h x ->
                 ListBackupsResponse' <$>
                   (x .?> "BackupSummaries" .!@ mempty) <*>
                     (x .?> "LastEvaluatedBackupArn")
                     <*> (pure (fromEnum s)))

instance Hashable ListBackups where

instance NFData ListBackups where

instance ToHeaders ListBackups where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.ListBackups" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON ListBackups where
        toJSON ListBackups'{..}
          = object
              (catMaybes
                 [("TimeRangeUpperBound" .=) <$>
                    _lbTimeRangeUpperBound,
                  ("TimeRangeLowerBound" .=) <$>
                    _lbTimeRangeLowerBound,
                  ("Limit" .=) <$> _lbLimit,
                  ("ExclusiveStartBackupArn" .=) <$>
                    _lbExclusiveStartBackupARN,
                  ("TableName" .=) <$> _lbTableName])

instance ToPath ListBackups where
        toPath = const "/"

instance ToQuery ListBackups where
        toQuery = const mempty

-- | /See:/ 'listBackupsResponse' smart constructor.
data ListBackupsResponse = ListBackupsResponse'
  { _lbrsBackupSummaries        :: !(Maybe [BackupSummary])
  , _lbrsLastEvaluatedBackupARN :: !(Maybe Text)
  , _lbrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListBackupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbrsBackupSummaries' - List of @BackupSummary@ objects.
--
-- * 'lbrsLastEvaluatedBackupARN' - Last evaluated BackupARN.
--
-- * 'lbrsResponseStatus' - -- | The response status code.
listBackupsResponse
    :: Int -- ^ 'lbrsResponseStatus'
    -> ListBackupsResponse
listBackupsResponse pResponseStatus_ =
  ListBackupsResponse'
    { _lbrsBackupSummaries = Nothing
    , _lbrsLastEvaluatedBackupARN = Nothing
    , _lbrsResponseStatus = pResponseStatus_
    }


-- | List of @BackupSummary@ objects.
lbrsBackupSummaries :: Lens' ListBackupsResponse [BackupSummary]
lbrsBackupSummaries = lens _lbrsBackupSummaries (\ s a -> s{_lbrsBackupSummaries = a}) . _Default . _Coerce

-- | Last evaluated BackupARN.
lbrsLastEvaluatedBackupARN :: Lens' ListBackupsResponse (Maybe Text)
lbrsLastEvaluatedBackupARN = lens _lbrsLastEvaluatedBackupARN (\ s a -> s{_lbrsLastEvaluatedBackupARN = a})

-- | -- | The response status code.
lbrsResponseStatus :: Lens' ListBackupsResponse Int
lbrsResponseStatus = lens _lbrsResponseStatus (\ s a -> s{_lbrsResponseStatus = a})

instance NFData ListBackupsResponse where
