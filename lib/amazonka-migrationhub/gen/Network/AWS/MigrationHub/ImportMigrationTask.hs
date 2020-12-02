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
-- Module      : Network.AWS.MigrationHub.ImportMigrationTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a new migration task which represents a server, database, etc., being migrated to AWS by a migration tool.
--
--
-- This API is a prerequisite to calling the @NotifyMigrationTaskState@ API as the migration tool must first register the migration task with Migration Hub.
--
module Network.AWS.MigrationHub.ImportMigrationTask
    (
    -- * Creating a Request
      importMigrationTask
    , ImportMigrationTask
    -- * Request Lenses
    , imtDryRun
    , imtProgressUpdateStream
    , imtMigrationTaskName

    -- * Destructuring the Response
    , importMigrationTaskResponse
    , ImportMigrationTaskResponse
    -- * Response Lenses
    , imtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MigrationHub.Types
import Network.AWS.MigrationHub.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'importMigrationTask' smart constructor.
data ImportMigrationTask = ImportMigrationTask'
  { _imtDryRun               :: !(Maybe Bool)
  , _imtProgressUpdateStream :: !Text
  , _imtMigrationTaskName    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportMigrationTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imtDryRun' - Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- * 'imtProgressUpdateStream' - The name of the ProgressUpdateStream.
--
-- * 'imtMigrationTaskName' - Unique identifier that references the migration task.
importMigrationTask
    :: Text -- ^ 'imtProgressUpdateStream'
    -> Text -- ^ 'imtMigrationTaskName'
    -> ImportMigrationTask
importMigrationTask pProgressUpdateStream_ pMigrationTaskName_ =
  ImportMigrationTask'
    { _imtDryRun = Nothing
    , _imtProgressUpdateStream = pProgressUpdateStream_
    , _imtMigrationTaskName = pMigrationTaskName_
    }


-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
imtDryRun :: Lens' ImportMigrationTask (Maybe Bool)
imtDryRun = lens _imtDryRun (\ s a -> s{_imtDryRun = a})

-- | The name of the ProgressUpdateStream.
imtProgressUpdateStream :: Lens' ImportMigrationTask Text
imtProgressUpdateStream = lens _imtProgressUpdateStream (\ s a -> s{_imtProgressUpdateStream = a})

-- | Unique identifier that references the migration task.
imtMigrationTaskName :: Lens' ImportMigrationTask Text
imtMigrationTaskName = lens _imtMigrationTaskName (\ s a -> s{_imtMigrationTaskName = a})

instance AWSRequest ImportMigrationTask where
        type Rs ImportMigrationTask =
             ImportMigrationTaskResponse
        request = postJSON migrationHub
        response
          = receiveEmpty
              (\ s h x ->
                 ImportMigrationTaskResponse' <$> (pure (fromEnum s)))

instance Hashable ImportMigrationTask where

instance NFData ImportMigrationTask where

instance ToHeaders ImportMigrationTask where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSMigrationHub.ImportMigrationTask" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ImportMigrationTask where
        toJSON ImportMigrationTask'{..}
          = object
              (catMaybes
                 [("DryRun" .=) <$> _imtDryRun,
                  Just
                    ("ProgressUpdateStream" .= _imtProgressUpdateStream),
                  Just ("MigrationTaskName" .= _imtMigrationTaskName)])

instance ToPath ImportMigrationTask where
        toPath = const "/"

instance ToQuery ImportMigrationTask where
        toQuery = const mempty

-- | /See:/ 'importMigrationTaskResponse' smart constructor.
newtype ImportMigrationTaskResponse = ImportMigrationTaskResponse'
  { _imtrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportMigrationTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'imtrsResponseStatus' - -- | The response status code.
importMigrationTaskResponse
    :: Int -- ^ 'imtrsResponseStatus'
    -> ImportMigrationTaskResponse
importMigrationTaskResponse pResponseStatus_ =
  ImportMigrationTaskResponse' {_imtrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
imtrsResponseStatus :: Lens' ImportMigrationTaskResponse Int
imtrsResponseStatus = lens _imtrsResponseStatus (\ s a -> s{_imtrsResponseStatus = a})

instance NFData ImportMigrationTaskResponse where
