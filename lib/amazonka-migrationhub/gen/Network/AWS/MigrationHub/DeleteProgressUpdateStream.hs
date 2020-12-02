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
-- Module      : Network.AWS.MigrationHub.DeleteProgressUpdateStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a progress update stream, including all of its tasks, which was previously created as an AWS resource used for access control. This API has the following traits:
--
--
--     * The only parameter needed for @DeleteProgressUpdateStream@ is the stream name (same as a @CreateProgressUpdateStream@ call).
--
--     * The call will return, and a background process will asynchronously delete the stream and all of its resources (tasks, associated resources, resource attributes, created artifacts).
--
--     * If the stream takes time to be deleted, it might still show up on a @ListProgressUpdateStreams@ call.
--
--     * @CreateProgressUpdateStream@ , @ImportMigrationTask@ , @NotifyMigrationTaskState@ , and all Associate[*] APIs realted to the tasks belonging to the stream will throw "InvalidInputException" if the stream of the same name is in the process of being deleted.
--
--     * Once the stream and all of its resources are deleted, @CreateProgressUpdateStream@ for a stream of the same name will succeed, and that stream will be an entirely new logical resource (without any resources associated with the old stream).
--
--
--
module Network.AWS.MigrationHub.DeleteProgressUpdateStream
    (
    -- * Creating a Request
      deleteProgressUpdateStream
    , DeleteProgressUpdateStream
    -- * Request Lenses
    , dpusDryRun
    , dpusProgressUpdateStreamName

    -- * Destructuring the Response
    , deleteProgressUpdateStreamResponse
    , DeleteProgressUpdateStreamResponse
    -- * Response Lenses
    , dpusrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MigrationHub.Types
import Network.AWS.MigrationHub.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteProgressUpdateStream' smart constructor.
data DeleteProgressUpdateStream = DeleteProgressUpdateStream'
  { _dpusDryRun                   :: !(Maybe Bool)
  , _dpusProgressUpdateStreamName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteProgressUpdateStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpusDryRun' - Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- * 'dpusProgressUpdateStreamName' - The name of the ProgressUpdateStream.
deleteProgressUpdateStream
    :: Text -- ^ 'dpusProgressUpdateStreamName'
    -> DeleteProgressUpdateStream
deleteProgressUpdateStream pProgressUpdateStreamName_ =
  DeleteProgressUpdateStream'
    { _dpusDryRun = Nothing
    , _dpusProgressUpdateStreamName = pProgressUpdateStreamName_
    }


-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
dpusDryRun :: Lens' DeleteProgressUpdateStream (Maybe Bool)
dpusDryRun = lens _dpusDryRun (\ s a -> s{_dpusDryRun = a})

-- | The name of the ProgressUpdateStream.
dpusProgressUpdateStreamName :: Lens' DeleteProgressUpdateStream Text
dpusProgressUpdateStreamName = lens _dpusProgressUpdateStreamName (\ s a -> s{_dpusProgressUpdateStreamName = a})

instance AWSRequest DeleteProgressUpdateStream where
        type Rs DeleteProgressUpdateStream =
             DeleteProgressUpdateStreamResponse
        request = postJSON migrationHub
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteProgressUpdateStreamResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteProgressUpdateStream where

instance NFData DeleteProgressUpdateStream where

instance ToHeaders DeleteProgressUpdateStream where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSMigrationHub.DeleteProgressUpdateStream" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteProgressUpdateStream where
        toJSON DeleteProgressUpdateStream'{..}
          = object
              (catMaybes
                 [("DryRun" .=) <$> _dpusDryRun,
                  Just
                    ("ProgressUpdateStreamName" .=
                       _dpusProgressUpdateStreamName)])

instance ToPath DeleteProgressUpdateStream where
        toPath = const "/"

instance ToQuery DeleteProgressUpdateStream where
        toQuery = const mempty

-- | /See:/ 'deleteProgressUpdateStreamResponse' smart constructor.
newtype DeleteProgressUpdateStreamResponse = DeleteProgressUpdateStreamResponse'
  { _dpusrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteProgressUpdateStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpusrsResponseStatus' - -- | The response status code.
deleteProgressUpdateStreamResponse
    :: Int -- ^ 'dpusrsResponseStatus'
    -> DeleteProgressUpdateStreamResponse
deleteProgressUpdateStreamResponse pResponseStatus_ =
  DeleteProgressUpdateStreamResponse' {_dpusrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dpusrsResponseStatus :: Lens' DeleteProgressUpdateStreamResponse Int
dpusrsResponseStatus = lens _dpusrsResponseStatus (\ s a -> s{_dpusrsResponseStatus = a})

instance NFData DeleteProgressUpdateStreamResponse
         where
