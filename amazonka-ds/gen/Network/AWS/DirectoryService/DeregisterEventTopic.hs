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

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Removes the specified directory as a publisher to the specified SNS
-- topic.
--
-- /See:/ 'newDeregisterEventTopic' smart constructor.
data DeregisterEventTopic = DeregisterEventTopic'
  { -- | The Directory ID to remove as a publisher. This directory will no longer
    -- send messages to the specified SNS topic.
    directoryId :: Core.Text,
    -- | The name of the SNS topic from which to remove the directory as a
    -- publisher.
    topicName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'topicName'
  Core.Text ->
  DeregisterEventTopic
newDeregisterEventTopic pDirectoryId_ pTopicName_ =
  DeregisterEventTopic'
    { directoryId = pDirectoryId_,
      topicName = pTopicName_
    }

-- | The Directory ID to remove as a publisher. This directory will no longer
-- send messages to the specified SNS topic.
deregisterEventTopic_directoryId :: Lens.Lens' DeregisterEventTopic Core.Text
deregisterEventTopic_directoryId = Lens.lens (\DeregisterEventTopic' {directoryId} -> directoryId) (\s@DeregisterEventTopic' {} a -> s {directoryId = a} :: DeregisterEventTopic)

-- | The name of the SNS topic from which to remove the directory as a
-- publisher.
deregisterEventTopic_topicName :: Lens.Lens' DeregisterEventTopic Core.Text
deregisterEventTopic_topicName = Lens.lens (\DeregisterEventTopic' {topicName} -> topicName) (\s@DeregisterEventTopic' {} a -> s {topicName = a} :: DeregisterEventTopic)

instance Core.AWSRequest DeregisterEventTopic where
  type
    AWSResponse DeregisterEventTopic =
      DeregisterEventTopicResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterEventTopicResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeregisterEventTopic

instance Core.NFData DeregisterEventTopic

instance Core.ToHeaders DeregisterEventTopic where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.DeregisterEventTopic" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeregisterEventTopic where
  toJSON DeregisterEventTopic' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DirectoryId" Core..= directoryId),
            Core.Just ("TopicName" Core..= topicName)
          ]
      )

instance Core.ToPath DeregisterEventTopic where
  toPath = Core.const "/"

instance Core.ToQuery DeregisterEventTopic where
  toQuery = Core.const Core.mempty

-- | The result of a DeregisterEventTopic request.
--
-- /See:/ 'newDeregisterEventTopicResponse' smart constructor.
data DeregisterEventTopicResponse = DeregisterEventTopicResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeregisterEventTopicResponse
newDeregisterEventTopicResponse pHttpStatus_ =
  DeregisterEventTopicResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deregisterEventTopicResponse_httpStatus :: Lens.Lens' DeregisterEventTopicResponse Core.Int
deregisterEventTopicResponse_httpStatus = Lens.lens (\DeregisterEventTopicResponse' {httpStatus} -> httpStatus) (\s@DeregisterEventTopicResponse' {} a -> s {httpStatus = a} :: DeregisterEventTopicResponse)

instance Core.NFData DeregisterEventTopicResponse
