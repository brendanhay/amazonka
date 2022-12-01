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
-- Module      : Amazonka.DirectoryService.RegisterEventTopic
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a directory with an Amazon SNS topic. This establishes the
-- directory as a publisher to the specified Amazon SNS topic. You can then
-- receive email or text (SMS) messages when the status of your directory
-- changes. You get notified if your directory goes from an Active status
-- to an Impaired or Inoperable status. You also receive a notification
-- when the directory returns to an Active status.
module Amazonka.DirectoryService.RegisterEventTopic
  ( -- * Creating a Request
    RegisterEventTopic (..),
    newRegisterEventTopic,

    -- * Request Lenses
    registerEventTopic_directoryId,
    registerEventTopic_topicName,

    -- * Destructuring the Response
    RegisterEventTopicResponse (..),
    newRegisterEventTopicResponse,

    -- * Response Lenses
    registerEventTopicResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Registers a new event topic.
--
-- /See:/ 'newRegisterEventTopic' smart constructor.
data RegisterEventTopic = RegisterEventTopic'
  { -- | The Directory ID that will publish status messages to the Amazon SNS
    -- topic.
    directoryId :: Prelude.Text,
    -- | The Amazon SNS topic name to which the directory will publish status
    -- messages. This Amazon SNS topic must be in the same region as the
    -- specified Directory ID.
    topicName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterEventTopic' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'registerEventTopic_directoryId' - The Directory ID that will publish status messages to the Amazon SNS
-- topic.
--
-- 'topicName', 'registerEventTopic_topicName' - The Amazon SNS topic name to which the directory will publish status
-- messages. This Amazon SNS topic must be in the same region as the
-- specified Directory ID.
newRegisterEventTopic ::
  -- | 'directoryId'
  Prelude.Text ->
  -- | 'topicName'
  Prelude.Text ->
  RegisterEventTopic
newRegisterEventTopic pDirectoryId_ pTopicName_ =
  RegisterEventTopic'
    { directoryId = pDirectoryId_,
      topicName = pTopicName_
    }

-- | The Directory ID that will publish status messages to the Amazon SNS
-- topic.
registerEventTopic_directoryId :: Lens.Lens' RegisterEventTopic Prelude.Text
registerEventTopic_directoryId = Lens.lens (\RegisterEventTopic' {directoryId} -> directoryId) (\s@RegisterEventTopic' {} a -> s {directoryId = a} :: RegisterEventTopic)

-- | The Amazon SNS topic name to which the directory will publish status
-- messages. This Amazon SNS topic must be in the same region as the
-- specified Directory ID.
registerEventTopic_topicName :: Lens.Lens' RegisterEventTopic Prelude.Text
registerEventTopic_topicName = Lens.lens (\RegisterEventTopic' {topicName} -> topicName) (\s@RegisterEventTopic' {} a -> s {topicName = a} :: RegisterEventTopic)

instance Core.AWSRequest RegisterEventTopic where
  type
    AWSResponse RegisterEventTopic =
      RegisterEventTopicResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RegisterEventTopicResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterEventTopic where
  hashWithSalt _salt RegisterEventTopic' {..} =
    _salt `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` topicName

instance Prelude.NFData RegisterEventTopic where
  rnf RegisterEventTopic' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf topicName

instance Core.ToHeaders RegisterEventTopic where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.RegisterEventTopic" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON RegisterEventTopic where
  toJSON RegisterEventTopic' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Core..= directoryId),
            Prelude.Just ("TopicName" Core..= topicName)
          ]
      )

instance Core.ToPath RegisterEventTopic where
  toPath = Prelude.const "/"

instance Core.ToQuery RegisterEventTopic where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a RegisterEventTopic request.
--
-- /See:/ 'newRegisterEventTopicResponse' smart constructor.
data RegisterEventTopicResponse = RegisterEventTopicResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterEventTopicResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'registerEventTopicResponse_httpStatus' - The response's http status code.
newRegisterEventTopicResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterEventTopicResponse
newRegisterEventTopicResponse pHttpStatus_ =
  RegisterEventTopicResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
registerEventTopicResponse_httpStatus :: Lens.Lens' RegisterEventTopicResponse Prelude.Int
registerEventTopicResponse_httpStatus = Lens.lens (\RegisterEventTopicResponse' {httpStatus} -> httpStatus) (\s@RegisterEventTopicResponse' {} a -> s {httpStatus = a} :: RegisterEventTopicResponse)

instance Prelude.NFData RegisterEventTopicResponse where
  rnf RegisterEventTopicResponse' {..} =
    Prelude.rnf httpStatus
