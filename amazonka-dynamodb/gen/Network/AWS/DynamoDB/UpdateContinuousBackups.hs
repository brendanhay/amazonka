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
-- Module      : Network.AWS.DynamoDB.UpdateContinuousBackups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @UpdateContinuousBackups@ enables or disables point in time recovery for the specified table. A successful @UpdateContinuousBackups@ call returns the current @ContinuousBackupsDescription@ . Continuous backups are @ENABLED@ on all tables at table creation. If point in time recovery is enabled, @PointInTimeRecoveryStatus@ will be set to ENABLED.
--
--
-- Once continuous backups and point in time recovery are enabled, you can restore to any point in time within @EarliestRestorableDateTime@ and @LatestRestorableDateTime@ .
--
-- @LatestRestorableDateTime@ is typically 5 minutes before the current time. You can restore your table to any point in time during the last 35 days..
--
module Network.AWS.DynamoDB.UpdateContinuousBackups
    (
    -- * Creating a Request
      updateContinuousBackups
    , UpdateContinuousBackups
    -- * Request Lenses
    , ucbTableName
    , ucbPointInTimeRecoverySpecification

    -- * Destructuring the Response
    , updateContinuousBackupsResponse
    , UpdateContinuousBackupsResponse
    -- * Response Lenses
    , ucbrsContinuousBackupsDescription
    , ucbrsResponseStatus
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateContinuousBackups' smart constructor.
data UpdateContinuousBackups = UpdateContinuousBackups'
  { _ucbTableName                        :: !Text
  , _ucbPointInTimeRecoverySpecification :: !PointInTimeRecoverySpecification
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateContinuousBackups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucbTableName' - The name of the table.
--
-- * 'ucbPointInTimeRecoverySpecification' - Represents the settings used to enable point in time recovery.
updateContinuousBackups
    :: Text -- ^ 'ucbTableName'
    -> PointInTimeRecoverySpecification -- ^ 'ucbPointInTimeRecoverySpecification'
    -> UpdateContinuousBackups
updateContinuousBackups pTableName_ pPointInTimeRecoverySpecification_ =
  UpdateContinuousBackups'
    { _ucbTableName = pTableName_
    , _ucbPointInTimeRecoverySpecification = pPointInTimeRecoverySpecification_
    }


-- | The name of the table.
ucbTableName :: Lens' UpdateContinuousBackups Text
ucbTableName = lens _ucbTableName (\ s a -> s{_ucbTableName = a})

-- | Represents the settings used to enable point in time recovery.
ucbPointInTimeRecoverySpecification :: Lens' UpdateContinuousBackups PointInTimeRecoverySpecification
ucbPointInTimeRecoverySpecification = lens _ucbPointInTimeRecoverySpecification (\ s a -> s{_ucbPointInTimeRecoverySpecification = a})

instance AWSRequest UpdateContinuousBackups where
        type Rs UpdateContinuousBackups =
             UpdateContinuousBackupsResponse
        request = postJSON dynamoDB
        response
          = receiveJSON
              (\ s h x ->
                 UpdateContinuousBackupsResponse' <$>
                   (x .?> "ContinuousBackupsDescription") <*>
                     (pure (fromEnum s)))

instance Hashable UpdateContinuousBackups where

instance NFData UpdateContinuousBackups where

instance ToHeaders UpdateContinuousBackups where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.UpdateContinuousBackups" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON UpdateContinuousBackups where
        toJSON UpdateContinuousBackups'{..}
          = object
              (catMaybes
                 [Just ("TableName" .= _ucbTableName),
                  Just
                    ("PointInTimeRecoverySpecification" .=
                       _ucbPointInTimeRecoverySpecification)])

instance ToPath UpdateContinuousBackups where
        toPath = const "/"

instance ToQuery UpdateContinuousBackups where
        toQuery = const mempty

-- | /See:/ 'updateContinuousBackupsResponse' smart constructor.
data UpdateContinuousBackupsResponse = UpdateContinuousBackupsResponse'
  { _ucbrsContinuousBackupsDescription :: !(Maybe ContinuousBackupsDescription)
  , _ucbrsResponseStatus               :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateContinuousBackupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucbrsContinuousBackupsDescription' - Represents the continuous backups and point in time recovery settings on the table.
--
-- * 'ucbrsResponseStatus' - -- | The response status code.
updateContinuousBackupsResponse
    :: Int -- ^ 'ucbrsResponseStatus'
    -> UpdateContinuousBackupsResponse
updateContinuousBackupsResponse pResponseStatus_ =
  UpdateContinuousBackupsResponse'
    { _ucbrsContinuousBackupsDescription = Nothing
    , _ucbrsResponseStatus = pResponseStatus_
    }


-- | Represents the continuous backups and point in time recovery settings on the table.
ucbrsContinuousBackupsDescription :: Lens' UpdateContinuousBackupsResponse (Maybe ContinuousBackupsDescription)
ucbrsContinuousBackupsDescription = lens _ucbrsContinuousBackupsDescription (\ s a -> s{_ucbrsContinuousBackupsDescription = a})

-- | -- | The response status code.
ucbrsResponseStatus :: Lens' UpdateContinuousBackupsResponse Int
ucbrsResponseStatus = lens _ucbrsResponseStatus (\ s a -> s{_ucbrsResponseStatus = a})

instance NFData UpdateContinuousBackupsResponse where
