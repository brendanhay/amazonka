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
-- Module      : Network.AWS.MigrationHub.CreateProgressUpdateStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a progress update stream which is an AWS resource used for access control as well as a namespace for migration task names that is implicitly linked to your AWS account. It must uniquely identify the migration tool as it is used for all updates made by the tool; however, it does not need to be unique for each AWS account because it is scoped to the AWS account.
--
--
module Network.AWS.MigrationHub.CreateProgressUpdateStream
    (
    -- * Creating a Request
      createProgressUpdateStream
    , CreateProgressUpdateStream
    -- * Request Lenses
    , cpusDryRun
    , cpusProgressUpdateStreamName

    -- * Destructuring the Response
    , createProgressUpdateStreamResponse
    , CreateProgressUpdateStreamResponse
    -- * Response Lenses
    , cpusrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MigrationHub.Types
import Network.AWS.MigrationHub.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createProgressUpdateStream' smart constructor.
data CreateProgressUpdateStream = CreateProgressUpdateStream'
  { _cpusDryRun                   :: !(Maybe Bool)
  , _cpusProgressUpdateStreamName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateProgressUpdateStream' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpusDryRun' - Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- * 'cpusProgressUpdateStreamName' - The name of the ProgressUpdateStream.
createProgressUpdateStream
    :: Text -- ^ 'cpusProgressUpdateStreamName'
    -> CreateProgressUpdateStream
createProgressUpdateStream pProgressUpdateStreamName_ =
  CreateProgressUpdateStream'
    { _cpusDryRun = Nothing
    , _cpusProgressUpdateStreamName = pProgressUpdateStreamName_
    }


-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
cpusDryRun :: Lens' CreateProgressUpdateStream (Maybe Bool)
cpusDryRun = lens _cpusDryRun (\ s a -> s{_cpusDryRun = a})

-- | The name of the ProgressUpdateStream.
cpusProgressUpdateStreamName :: Lens' CreateProgressUpdateStream Text
cpusProgressUpdateStreamName = lens _cpusProgressUpdateStreamName (\ s a -> s{_cpusProgressUpdateStreamName = a})

instance AWSRequest CreateProgressUpdateStream where
        type Rs CreateProgressUpdateStream =
             CreateProgressUpdateStreamResponse
        request = postJSON migrationHub
        response
          = receiveEmpty
              (\ s h x ->
                 CreateProgressUpdateStreamResponse' <$>
                   (pure (fromEnum s)))

instance Hashable CreateProgressUpdateStream where

instance NFData CreateProgressUpdateStream where

instance ToHeaders CreateProgressUpdateStream where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSMigrationHub.CreateProgressUpdateStream" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateProgressUpdateStream where
        toJSON CreateProgressUpdateStream'{..}
          = object
              (catMaybes
                 [("DryRun" .=) <$> _cpusDryRun,
                  Just
                    ("ProgressUpdateStreamName" .=
                       _cpusProgressUpdateStreamName)])

instance ToPath CreateProgressUpdateStream where
        toPath = const "/"

instance ToQuery CreateProgressUpdateStream where
        toQuery = const mempty

-- | /See:/ 'createProgressUpdateStreamResponse' smart constructor.
newtype CreateProgressUpdateStreamResponse = CreateProgressUpdateStreamResponse'
  { _cpusrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateProgressUpdateStreamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpusrsResponseStatus' - -- | The response status code.
createProgressUpdateStreamResponse
    :: Int -- ^ 'cpusrsResponseStatus'
    -> CreateProgressUpdateStreamResponse
createProgressUpdateStreamResponse pResponseStatus_ =
  CreateProgressUpdateStreamResponse' {_cpusrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
cpusrsResponseStatus :: Lens' CreateProgressUpdateStreamResponse Int
cpusrsResponseStatus = lens _cpusrsResponseStatus (\ s a -> s{_cpusrsResponseStatus = a})

instance NFData CreateProgressUpdateStreamResponse
         where
