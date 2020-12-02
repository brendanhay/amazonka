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
-- Module      : Network.AWS.MigrationHub.NotifyMigrationTaskState
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Notifies Migration Hub of the current status, progress, or other detail regarding a migration task. This API has the following traits:
--
--
--     * Migration tools will call the @NotifyMigrationTaskState@ API to share the latest progress and status.
--
--     * @MigrationTaskName@ is used for addressing updates to the correct target.
--
--     * @ProgressUpdateStream@ is used for access control and to provide a namespace for each migration tool.
--
--
--
module Network.AWS.MigrationHub.NotifyMigrationTaskState
    (
    -- * Creating a Request
      notifyMigrationTaskState
    , NotifyMigrationTaskState
    -- * Request Lenses
    , nmtsDryRun
    , nmtsProgressUpdateStream
    , nmtsMigrationTaskName
    , nmtsTask
    , nmtsUpdateDateTime
    , nmtsNextUpdateSeconds

    -- * Destructuring the Response
    , notifyMigrationTaskStateResponse
    , NotifyMigrationTaskStateResponse
    -- * Response Lenses
    , nmtsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MigrationHub.Types
import Network.AWS.MigrationHub.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'notifyMigrationTaskState' smart constructor.
data NotifyMigrationTaskState = NotifyMigrationTaskState'
  { _nmtsDryRun               :: !(Maybe Bool)
  , _nmtsProgressUpdateStream :: !Text
  , _nmtsMigrationTaskName    :: !Text
  , _nmtsTask                 :: !Task
  , _nmtsUpdateDateTime       :: !POSIX
  , _nmtsNextUpdateSeconds    :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NotifyMigrationTaskState' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nmtsDryRun' - Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- * 'nmtsProgressUpdateStream' - The name of the ProgressUpdateStream.
--
-- * 'nmtsMigrationTaskName' - Unique identifier that references the migration task.
--
-- * 'nmtsTask' - Information about the task's progress and status.
--
-- * 'nmtsUpdateDateTime' - The timestamp when the task was gathered.
--
-- * 'nmtsNextUpdateSeconds' - Number of seconds after the UpdateDateTime within which the Migration Hub can expect an update. If Migration Hub does not receive an update within the specified interval, then the migration task will be considered stale.
notifyMigrationTaskState
    :: Text -- ^ 'nmtsProgressUpdateStream'
    -> Text -- ^ 'nmtsMigrationTaskName'
    -> Task -- ^ 'nmtsTask'
    -> UTCTime -- ^ 'nmtsUpdateDateTime'
    -> Natural -- ^ 'nmtsNextUpdateSeconds'
    -> NotifyMigrationTaskState
notifyMigrationTaskState pProgressUpdateStream_ pMigrationTaskName_ pTask_ pUpdateDateTime_ pNextUpdateSeconds_ =
  NotifyMigrationTaskState'
    { _nmtsDryRun = Nothing
    , _nmtsProgressUpdateStream = pProgressUpdateStream_
    , _nmtsMigrationTaskName = pMigrationTaskName_
    , _nmtsTask = pTask_
    , _nmtsUpdateDateTime = _Time # pUpdateDateTime_
    , _nmtsNextUpdateSeconds = _Nat # pNextUpdateSeconds_
    }


-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
nmtsDryRun :: Lens' NotifyMigrationTaskState (Maybe Bool)
nmtsDryRun = lens _nmtsDryRun (\ s a -> s{_nmtsDryRun = a})

-- | The name of the ProgressUpdateStream.
nmtsProgressUpdateStream :: Lens' NotifyMigrationTaskState Text
nmtsProgressUpdateStream = lens _nmtsProgressUpdateStream (\ s a -> s{_nmtsProgressUpdateStream = a})

-- | Unique identifier that references the migration task.
nmtsMigrationTaskName :: Lens' NotifyMigrationTaskState Text
nmtsMigrationTaskName = lens _nmtsMigrationTaskName (\ s a -> s{_nmtsMigrationTaskName = a})

-- | Information about the task's progress and status.
nmtsTask :: Lens' NotifyMigrationTaskState Task
nmtsTask = lens _nmtsTask (\ s a -> s{_nmtsTask = a})

-- | The timestamp when the task was gathered.
nmtsUpdateDateTime :: Lens' NotifyMigrationTaskState UTCTime
nmtsUpdateDateTime = lens _nmtsUpdateDateTime (\ s a -> s{_nmtsUpdateDateTime = a}) . _Time

-- | Number of seconds after the UpdateDateTime within which the Migration Hub can expect an update. If Migration Hub does not receive an update within the specified interval, then the migration task will be considered stale.
nmtsNextUpdateSeconds :: Lens' NotifyMigrationTaskState Natural
nmtsNextUpdateSeconds = lens _nmtsNextUpdateSeconds (\ s a -> s{_nmtsNextUpdateSeconds = a}) . _Nat

instance AWSRequest NotifyMigrationTaskState where
        type Rs NotifyMigrationTaskState =
             NotifyMigrationTaskStateResponse
        request = postJSON migrationHub
        response
          = receiveEmpty
              (\ s h x ->
                 NotifyMigrationTaskStateResponse' <$>
                   (pure (fromEnum s)))

instance Hashable NotifyMigrationTaskState where

instance NFData NotifyMigrationTaskState where

instance ToHeaders NotifyMigrationTaskState where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSMigrationHub.NotifyMigrationTaskState" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON NotifyMigrationTaskState where
        toJSON NotifyMigrationTaskState'{..}
          = object
              (catMaybes
                 [("DryRun" .=) <$> _nmtsDryRun,
                  Just
                    ("ProgressUpdateStream" .=
                       _nmtsProgressUpdateStream),
                  Just ("MigrationTaskName" .= _nmtsMigrationTaskName),
                  Just ("Task" .= _nmtsTask),
                  Just ("UpdateDateTime" .= _nmtsUpdateDateTime),
                  Just
                    ("NextUpdateSeconds" .= _nmtsNextUpdateSeconds)])

instance ToPath NotifyMigrationTaskState where
        toPath = const "/"

instance ToQuery NotifyMigrationTaskState where
        toQuery = const mempty

-- | /See:/ 'notifyMigrationTaskStateResponse' smart constructor.
newtype NotifyMigrationTaskStateResponse = NotifyMigrationTaskStateResponse'
  { _nmtsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NotifyMigrationTaskStateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nmtsrsResponseStatus' - -- | The response status code.
notifyMigrationTaskStateResponse
    :: Int -- ^ 'nmtsrsResponseStatus'
    -> NotifyMigrationTaskStateResponse
notifyMigrationTaskStateResponse pResponseStatus_ =
  NotifyMigrationTaskStateResponse' {_nmtsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
nmtsrsResponseStatus :: Lens' NotifyMigrationTaskStateResponse Int
nmtsrsResponseStatus = lens _nmtsrsResponseStatus (\ s a -> s{_nmtsrsResponseStatus = a})

instance NFData NotifyMigrationTaskStateResponse
         where
