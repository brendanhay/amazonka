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
-- Module      : Network.AWS.DynamoDB.RestoreTableToPointInTime
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores the specified table to the specified point in time within @EarliestRestorableDateTime@ and @LatestRestorableDateTime@ . You can restore your table to any point in time during the last 35 days. Any number of users can execute up to 4 concurrent restores (any type of restore) in a given account.
--
--
-- When you restore using point in time recovery, DynamoDB restores your table data to the state based on the selected date and time (day:hour:minute:second) to a new table.
--
-- Along with data, the following are also included on the new restored table using point in time recovery:
--
--     * Global secondary indexes (GSIs)
--
--     * Local secondary indexes (LSIs)
--
--     * Provisioned read and write capacity
--
--     * Encryption settings
--
-- /Important:/ All these settings come from the current settings of the source table at the time of restore.
--
--
--
-- You must manually set up the following on the restored table:
--
--     * Auto scaling policies
--
--     * IAM policies
--
--     * Cloudwatch metrics and alarms
--
--     * Tags
--
--     * Stream settings
--
--     * Time to Live (TTL) settings
--
--     * Point in time recovery settings
--
--
--
module Network.AWS.DynamoDB.RestoreTableToPointInTime
    (
    -- * Creating a Request
      restoreTableToPointInTime
    , RestoreTableToPointInTime
    -- * Request Lenses
    , rttpitUseLatestRestorableTime
    , rttpitRestoreDateTime
    , rttpitSourceTableName
    , rttpitTargetTableName

    -- * Destructuring the Response
    , restoreTableToPointInTimeResponse
    , RestoreTableToPointInTimeResponse
    -- * Response Lenses
    , rttpitrsTableDescription
    , rttpitrsResponseStatus
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'restoreTableToPointInTime' smart constructor.
data RestoreTableToPointInTime = RestoreTableToPointInTime'
  { _rttpitUseLatestRestorableTime :: !(Maybe Bool)
  , _rttpitRestoreDateTime         :: !(Maybe POSIX)
  , _rttpitSourceTableName         :: !Text
  , _rttpitTargetTableName         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreTableToPointInTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rttpitUseLatestRestorableTime' - Restore the table to the latest possible time. @LatestRestorableDateTime@ is typically 5 minutes before the current time.
--
-- * 'rttpitRestoreDateTime' - Time in the past to restore the table to.
--
-- * 'rttpitSourceTableName' - Name of the source table that is being restored.
--
-- * 'rttpitTargetTableName' - The name of the new table to which it must be restored to.
restoreTableToPointInTime
    :: Text -- ^ 'rttpitSourceTableName'
    -> Text -- ^ 'rttpitTargetTableName'
    -> RestoreTableToPointInTime
restoreTableToPointInTime pSourceTableName_ pTargetTableName_ =
  RestoreTableToPointInTime'
    { _rttpitUseLatestRestorableTime = Nothing
    , _rttpitRestoreDateTime = Nothing
    , _rttpitSourceTableName = pSourceTableName_
    , _rttpitTargetTableName = pTargetTableName_
    }


-- | Restore the table to the latest possible time. @LatestRestorableDateTime@ is typically 5 minutes before the current time.
rttpitUseLatestRestorableTime :: Lens' RestoreTableToPointInTime (Maybe Bool)
rttpitUseLatestRestorableTime = lens _rttpitUseLatestRestorableTime (\ s a -> s{_rttpitUseLatestRestorableTime = a})

-- | Time in the past to restore the table to.
rttpitRestoreDateTime :: Lens' RestoreTableToPointInTime (Maybe UTCTime)
rttpitRestoreDateTime = lens _rttpitRestoreDateTime (\ s a -> s{_rttpitRestoreDateTime = a}) . mapping _Time

-- | Name of the source table that is being restored.
rttpitSourceTableName :: Lens' RestoreTableToPointInTime Text
rttpitSourceTableName = lens _rttpitSourceTableName (\ s a -> s{_rttpitSourceTableName = a})

-- | The name of the new table to which it must be restored to.
rttpitTargetTableName :: Lens' RestoreTableToPointInTime Text
rttpitTargetTableName = lens _rttpitTargetTableName (\ s a -> s{_rttpitTargetTableName = a})

instance AWSRequest RestoreTableToPointInTime where
        type Rs RestoreTableToPointInTime =
             RestoreTableToPointInTimeResponse
        request = postJSON dynamoDB
        response
          = receiveJSON
              (\ s h x ->
                 RestoreTableToPointInTimeResponse' <$>
                   (x .?> "TableDescription") <*> (pure (fromEnum s)))

instance Hashable RestoreTableToPointInTime where

instance NFData RestoreTableToPointInTime where

instance ToHeaders RestoreTableToPointInTime where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.RestoreTableToPointInTime" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON RestoreTableToPointInTime where
        toJSON RestoreTableToPointInTime'{..}
          = object
              (catMaybes
                 [("UseLatestRestorableTime" .=) <$>
                    _rttpitUseLatestRestorableTime,
                  ("RestoreDateTime" .=) <$> _rttpitRestoreDateTime,
                  Just ("SourceTableName" .= _rttpitSourceTableName),
                  Just ("TargetTableName" .= _rttpitTargetTableName)])

instance ToPath RestoreTableToPointInTime where
        toPath = const "/"

instance ToQuery RestoreTableToPointInTime where
        toQuery = const mempty

-- | /See:/ 'restoreTableToPointInTimeResponse' smart constructor.
data RestoreTableToPointInTimeResponse = RestoreTableToPointInTimeResponse'
  { _rttpitrsTableDescription :: !(Maybe TableDescription)
  , _rttpitrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreTableToPointInTimeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rttpitrsTableDescription' - Represents the properties of a table.
--
-- * 'rttpitrsResponseStatus' - -- | The response status code.
restoreTableToPointInTimeResponse
    :: Int -- ^ 'rttpitrsResponseStatus'
    -> RestoreTableToPointInTimeResponse
restoreTableToPointInTimeResponse pResponseStatus_ =
  RestoreTableToPointInTimeResponse'
    { _rttpitrsTableDescription = Nothing
    , _rttpitrsResponseStatus = pResponseStatus_
    }


-- | Represents the properties of a table.
rttpitrsTableDescription :: Lens' RestoreTableToPointInTimeResponse (Maybe TableDescription)
rttpitrsTableDescription = lens _rttpitrsTableDescription (\ s a -> s{_rttpitrsTableDescription = a})

-- | -- | The response status code.
rttpitrsResponseStatus :: Lens' RestoreTableToPointInTimeResponse Int
rttpitrsResponseStatus = lens _rttpitrsResponseStatus (\ s a -> s{_rttpitrsResponseStatus = a})

instance NFData RestoreTableToPointInTimeResponse
         where
