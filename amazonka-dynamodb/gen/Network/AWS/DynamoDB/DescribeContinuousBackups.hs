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
-- Module      : Network.AWS.DynamoDB.DescribeContinuousBackups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Checks the status of continuous backups and point in time recovery on the specified table. Continuous backups are @ENABLED@ on all tables at table creation. If point in time recovery is enabled, @PointInTimeRecoveryStatus@ will be set to ENABLED.
--
--
-- Once continuous backups and point in time recovery are enabled, you can restore to any point in time within @EarliestRestorableDateTime@ and @LatestRestorableDateTime@ .
--
-- @LatestRestorableDateTime@ is typically 5 minutes before the current time. You can restore your table to any point in time during the last 35 days.
--
-- You can call @DescribeContinuousBackups@ at a maximum rate of 10 times per second.
--
module Network.AWS.DynamoDB.DescribeContinuousBackups
    (
    -- * Creating a Request
      describeContinuousBackups
    , DescribeContinuousBackups
    -- * Request Lenses
    , dcbTableName

    -- * Destructuring the Response
    , describeContinuousBackupsResponse
    , DescribeContinuousBackupsResponse
    -- * Response Lenses
    , dcbrsContinuousBackupsDescription
    , dcbrsResponseStatus
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeContinuousBackups' smart constructor.
newtype DescribeContinuousBackups = DescribeContinuousBackups'
  { _dcbTableName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeContinuousBackups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcbTableName' - Name of the table for which the customer wants to check the continuous backups and point in time recovery settings.
describeContinuousBackups
    :: Text -- ^ 'dcbTableName'
    -> DescribeContinuousBackups
describeContinuousBackups pTableName_ =
  DescribeContinuousBackups' {_dcbTableName = pTableName_}


-- | Name of the table for which the customer wants to check the continuous backups and point in time recovery settings.
dcbTableName :: Lens' DescribeContinuousBackups Text
dcbTableName = lens _dcbTableName (\ s a -> s{_dcbTableName = a})

instance AWSRequest DescribeContinuousBackups where
        type Rs DescribeContinuousBackups =
             DescribeContinuousBackupsResponse
        request = postJSON dynamoDB
        response
          = receiveJSON
              (\ s h x ->
                 DescribeContinuousBackupsResponse' <$>
                   (x .?> "ContinuousBackupsDescription") <*>
                     (pure (fromEnum s)))

instance Hashable DescribeContinuousBackups where

instance NFData DescribeContinuousBackups where

instance ToHeaders DescribeContinuousBackups where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.DescribeContinuousBackups" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON DescribeContinuousBackups where
        toJSON DescribeContinuousBackups'{..}
          = object
              (catMaybes [Just ("TableName" .= _dcbTableName)])

instance ToPath DescribeContinuousBackups where
        toPath = const "/"

instance ToQuery DescribeContinuousBackups where
        toQuery = const mempty

-- | /See:/ 'describeContinuousBackupsResponse' smart constructor.
data DescribeContinuousBackupsResponse = DescribeContinuousBackupsResponse'
  { _dcbrsContinuousBackupsDescription :: !(Maybe ContinuousBackupsDescription)
  , _dcbrsResponseStatus               :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeContinuousBackupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcbrsContinuousBackupsDescription' - @ContinuousBackupsDescription@ can be one of the following : ENABLED, DISABLED.
--
-- * 'dcbrsResponseStatus' - -- | The response status code.
describeContinuousBackupsResponse
    :: Int -- ^ 'dcbrsResponseStatus'
    -> DescribeContinuousBackupsResponse
describeContinuousBackupsResponse pResponseStatus_ =
  DescribeContinuousBackupsResponse'
    { _dcbrsContinuousBackupsDescription = Nothing
    , _dcbrsResponseStatus = pResponseStatus_
    }


-- | @ContinuousBackupsDescription@ can be one of the following : ENABLED, DISABLED.
dcbrsContinuousBackupsDescription :: Lens' DescribeContinuousBackupsResponse (Maybe ContinuousBackupsDescription)
dcbrsContinuousBackupsDescription = lens _dcbrsContinuousBackupsDescription (\ s a -> s{_dcbrsContinuousBackupsDescription = a})

-- | -- | The response status code.
dcbrsResponseStatus :: Lens' DescribeContinuousBackupsResponse Int
dcbrsResponseStatus = lens _dcbrsResponseStatus (\ s a -> s{_dcbrsResponseStatus = a})

instance NFData DescribeContinuousBackupsResponse
         where
