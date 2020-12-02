{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.CreateLogSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a subscription to forward real-time Directory Service domain controller security logs to the specified Amazon CloudWatch log group in your AWS account.
module Network.AWS.DirectoryService.CreateLogSubscription
  ( -- * Creating a Request
    createLogSubscription,
    CreateLogSubscription,

    -- * Request Lenses
    clsDirectoryId,
    clsLogGroupName,

    -- * Destructuring the Response
    createLogSubscriptionResponse,
    CreateLogSubscriptionResponse,

    -- * Response Lenses
    clsrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createLogSubscription' smart constructor.
data CreateLogSubscription = CreateLogSubscription'
  { _clsDirectoryId ::
      !Text,
    _clsLogGroupName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateLogSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clsDirectoryId' - Identifier of the directory to which you want to subscribe and receive real-time logs to your specified CloudWatch log group.
--
-- * 'clsLogGroupName' - The name of the CloudWatch log group where the real-time domain controller logs are forwarded.
createLogSubscription ::
  -- | 'clsDirectoryId'
  Text ->
  -- | 'clsLogGroupName'
  Text ->
  CreateLogSubscription
createLogSubscription pDirectoryId_ pLogGroupName_ =
  CreateLogSubscription'
    { _clsDirectoryId = pDirectoryId_,
      _clsLogGroupName = pLogGroupName_
    }

-- | Identifier of the directory to which you want to subscribe and receive real-time logs to your specified CloudWatch log group.
clsDirectoryId :: Lens' CreateLogSubscription Text
clsDirectoryId = lens _clsDirectoryId (\s a -> s {_clsDirectoryId = a})

-- | The name of the CloudWatch log group where the real-time domain controller logs are forwarded.
clsLogGroupName :: Lens' CreateLogSubscription Text
clsLogGroupName = lens _clsLogGroupName (\s a -> s {_clsLogGroupName = a})

instance AWSRequest CreateLogSubscription where
  type Rs CreateLogSubscription = CreateLogSubscriptionResponse
  request = postJSON directoryService
  response =
    receiveEmpty
      (\s h x -> CreateLogSubscriptionResponse' <$> (pure (fromEnum s)))

instance Hashable CreateLogSubscription

instance NFData CreateLogSubscription

instance ToHeaders CreateLogSubscription where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DirectoryService_20150416.CreateLogSubscription" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateLogSubscription where
  toJSON CreateLogSubscription' {..} =
    object
      ( catMaybes
          [ Just ("DirectoryId" .= _clsDirectoryId),
            Just ("LogGroupName" .= _clsLogGroupName)
          ]
      )

instance ToPath CreateLogSubscription where
  toPath = const "/"

instance ToQuery CreateLogSubscription where
  toQuery = const mempty

-- | /See:/ 'createLogSubscriptionResponse' smart constructor.
newtype CreateLogSubscriptionResponse = CreateLogSubscriptionResponse'
  { _clsrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateLogSubscriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clsrsResponseStatus' - -- | The response status code.
createLogSubscriptionResponse ::
  -- | 'clsrsResponseStatus'
  Int ->
  CreateLogSubscriptionResponse
createLogSubscriptionResponse pResponseStatus_ =
  CreateLogSubscriptionResponse'
    { _clsrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
clsrsResponseStatus :: Lens' CreateLogSubscriptionResponse Int
clsrsResponseStatus = lens _clsrsResponseStatus (\s a -> s {_clsrsResponseStatus = a})

instance NFData CreateLogSubscriptionResponse
