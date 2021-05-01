{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DeregisterEventTopic
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified directory as a publisher to the specified SNS
-- topic.
module Network.AWS.DirectoryService.DeregisterEventTopic
  ( -- * Creating a Request
    DeregisterEventTopic (..),
    newDeregisterEventTopic,

    -- * Request Lenses
    deregisterEventTopic_directoryId,
    deregisterEventTopic_topicName,

    -- * Destructuring the Response
    DeregisterEventTopicResponse (..),
    newDeregisterEventTopicResponse,

    -- * Response Lenses
    deregisterEventTopicResponse_httpStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Removes the specified directory as a publisher to the specified SNS
-- topic.
--
-- /See:/ 'newDeregisterEventTopic' smart constructor.
data DeregisterEventTopic = DeregisterEventTopic'
  { -- | The Directory ID to remove as a publisher. This directory will no longer
    -- send messages to the specified SNS topic.
    directoryId :: Prelude.Text,
    -- | The name of the SNS topic from which to remove the directory as a
    -- publisher.
    topicName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeregisterEventTopic' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'deregisterEventTopic_directoryId' - The Directory ID to remove as a publisher. This directory will no longer
-- send messages to the specified SNS topic.
--
-- 'topicName', 'deregisterEventTopic_topicName' - The name of the SNS topic from which to remove the directory as a
-- publisher.
newDeregisterEventTopic ::
  -- | 'directoryId'
  Prelude.Text ->
  -- | 'topicName'
  Prelude.Text ->
  DeregisterEventTopic
newDeregisterEventTopic pDirectoryId_ pTopicName_ =
  DeregisterEventTopic'
    { directoryId = pDirectoryId_,
      topicName = pTopicName_
    }

-- | The Directory ID to remove as a publisher. This directory will no longer
-- send messages to the specified SNS topic.
deregisterEventTopic_directoryId :: Lens.Lens' DeregisterEventTopic Prelude.Text
deregisterEventTopic_directoryId = Lens.lens (\DeregisterEventTopic' {directoryId} -> directoryId) (\s@DeregisterEventTopic' {} a -> s {directoryId = a} :: DeregisterEventTopic)

-- | The name of the SNS topic from which to remove the directory as a
-- publisher.
deregisterEventTopic_topicName :: Lens.Lens' DeregisterEventTopic Prelude.Text
deregisterEventTopic_topicName = Lens.lens (\DeregisterEventTopic' {topicName} -> topicName) (\s@DeregisterEventTopic' {} a -> s {topicName = a} :: DeregisterEventTopic)

instance Prelude.AWSRequest DeregisterEventTopic where
  type
    Rs DeregisterEventTopic =
      DeregisterEventTopicResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterEventTopicResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterEventTopic

instance Prelude.NFData DeregisterEventTopic

instance Prelude.ToHeaders DeregisterEventTopic where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.DeregisterEventTopic" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeregisterEventTopic where
  toJSON DeregisterEventTopic' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Prelude..= directoryId),
            Prelude.Just ("TopicName" Prelude..= topicName)
          ]
      )

instance Prelude.ToPath DeregisterEventTopic where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeregisterEventTopic where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a DeregisterEventTopic request.
--
-- /See:/ 'newDeregisterEventTopicResponse' smart constructor.
data DeregisterEventTopicResponse = DeregisterEventTopicResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeregisterEventTopicResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deregisterEventTopicResponse_httpStatus' - The response's http status code.
newDeregisterEventTopicResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterEventTopicResponse
newDeregisterEventTopicResponse pHttpStatus_ =
  DeregisterEventTopicResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deregisterEventTopicResponse_httpStatus :: Lens.Lens' DeregisterEventTopicResponse Prelude.Int
deregisterEventTopicResponse_httpStatus = Lens.lens (\DeregisterEventTopicResponse' {httpStatus} -> httpStatus) (\s@DeregisterEventTopicResponse' {} a -> s {httpStatus = a} :: DeregisterEventTopicResponse)

instance Prelude.NFData DeregisterEventTopicResponse
