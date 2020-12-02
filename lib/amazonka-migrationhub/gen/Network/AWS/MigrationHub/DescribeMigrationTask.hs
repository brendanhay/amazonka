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
-- Module      : Network.AWS.MigrationHub.DescribeMigrationTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of all attributes associated with a specific migration task.
--
--
module Network.AWS.MigrationHub.DescribeMigrationTask
    (
    -- * Creating a Request
      describeMigrationTask
    , DescribeMigrationTask
    -- * Request Lenses
    , dmtProgressUpdateStream
    , dmtMigrationTaskName

    -- * Destructuring the Response
    , describeMigrationTaskResponse
    , DescribeMigrationTaskResponse
    -- * Response Lenses
    , dmtrsMigrationTask
    , dmtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MigrationHub.Types
import Network.AWS.MigrationHub.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeMigrationTask' smart constructor.
data DescribeMigrationTask = DescribeMigrationTask'
  { _dmtProgressUpdateStream :: !Text
  , _dmtMigrationTaskName    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMigrationTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmtProgressUpdateStream' - The name of the ProgressUpdateStream.
--
-- * 'dmtMigrationTaskName' - The identifier given to the MigrationTask.
describeMigrationTask
    :: Text -- ^ 'dmtProgressUpdateStream'
    -> Text -- ^ 'dmtMigrationTaskName'
    -> DescribeMigrationTask
describeMigrationTask pProgressUpdateStream_ pMigrationTaskName_ =
  DescribeMigrationTask'
    { _dmtProgressUpdateStream = pProgressUpdateStream_
    , _dmtMigrationTaskName = pMigrationTaskName_
    }


-- | The name of the ProgressUpdateStream.
dmtProgressUpdateStream :: Lens' DescribeMigrationTask Text
dmtProgressUpdateStream = lens _dmtProgressUpdateStream (\ s a -> s{_dmtProgressUpdateStream = a})

-- | The identifier given to the MigrationTask.
dmtMigrationTaskName :: Lens' DescribeMigrationTask Text
dmtMigrationTaskName = lens _dmtMigrationTaskName (\ s a -> s{_dmtMigrationTaskName = a})

instance AWSRequest DescribeMigrationTask where
        type Rs DescribeMigrationTask =
             DescribeMigrationTaskResponse
        request = postJSON migrationHub
        response
          = receiveJSON
              (\ s h x ->
                 DescribeMigrationTaskResponse' <$>
                   (x .?> "MigrationTask") <*> (pure (fromEnum s)))

instance Hashable DescribeMigrationTask where

instance NFData DescribeMigrationTask where

instance ToHeaders DescribeMigrationTask where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSMigrationHub.DescribeMigrationTask" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeMigrationTask where
        toJSON DescribeMigrationTask'{..}
          = object
              (catMaybes
                 [Just
                    ("ProgressUpdateStream" .= _dmtProgressUpdateStream),
                  Just ("MigrationTaskName" .= _dmtMigrationTaskName)])

instance ToPath DescribeMigrationTask where
        toPath = const "/"

instance ToQuery DescribeMigrationTask where
        toQuery = const mempty

-- | /See:/ 'describeMigrationTaskResponse' smart constructor.
data DescribeMigrationTaskResponse = DescribeMigrationTaskResponse'
  { _dmtrsMigrationTask  :: !(Maybe MigrationTask)
  , _dmtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMigrationTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmtrsMigrationTask' - Object encapsulating information about the migration task.
--
-- * 'dmtrsResponseStatus' - -- | The response status code.
describeMigrationTaskResponse
    :: Int -- ^ 'dmtrsResponseStatus'
    -> DescribeMigrationTaskResponse
describeMigrationTaskResponse pResponseStatus_ =
  DescribeMigrationTaskResponse'
    {_dmtrsMigrationTask = Nothing, _dmtrsResponseStatus = pResponseStatus_}


-- | Object encapsulating information about the migration task.
dmtrsMigrationTask :: Lens' DescribeMigrationTaskResponse (Maybe MigrationTask)
dmtrsMigrationTask = lens _dmtrsMigrationTask (\ s a -> s{_dmtrsMigrationTask = a})

-- | -- | The response status code.
dmtrsResponseStatus :: Lens' DescribeMigrationTaskResponse Int
dmtrsResponseStatus = lens _dmtrsResponseStatus (\ s a -> s{_dmtrsResponseStatus = a})

instance NFData DescribeMigrationTaskResponse where
