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
-- Module      : Amazonka.DirectoryService.DeregisterEventTopic
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified directory as a publisher to the specified Amazon
-- SNS topic.
module Amazonka.DirectoryService.DeregisterEventTopic
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Removes the specified directory as a publisher to the specified Amazon
-- SNS topic.
--
-- /See:/ 'newDeregisterEventTopic' smart constructor.
data DeregisterEventTopic = DeregisterEventTopic'
  { -- | The Directory ID to remove as a publisher. This directory will no longer
    -- send messages to the specified Amazon SNS topic.
    directoryId :: Prelude.Text,
    -- | The name of the Amazon SNS topic from which to remove the directory as a
    -- publisher.
    topicName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterEventTopic' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'deregisterEventTopic_directoryId' - The Directory ID to remove as a publisher. This directory will no longer
-- send messages to the specified Amazon SNS topic.
--
-- 'topicName', 'deregisterEventTopic_topicName' - The name of the Amazon SNS topic from which to remove the directory as a
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
-- send messages to the specified Amazon SNS topic.
deregisterEventTopic_directoryId :: Lens.Lens' DeregisterEventTopic Prelude.Text
deregisterEventTopic_directoryId = Lens.lens (\DeregisterEventTopic' {directoryId} -> directoryId) (\s@DeregisterEventTopic' {} a -> s {directoryId = a} :: DeregisterEventTopic)

-- | The name of the Amazon SNS topic from which to remove the directory as a
-- publisher.
deregisterEventTopic_topicName :: Lens.Lens' DeregisterEventTopic Prelude.Text
deregisterEventTopic_topicName = Lens.lens (\DeregisterEventTopic' {topicName} -> topicName) (\s@DeregisterEventTopic' {} a -> s {topicName = a} :: DeregisterEventTopic)

instance Core.AWSRequest DeregisterEventTopic where
  type
    AWSResponse DeregisterEventTopic =
      DeregisterEventTopicResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeregisterEventTopicResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterEventTopic where
  hashWithSalt _salt DeregisterEventTopic' {..} =
    _salt
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` topicName

instance Prelude.NFData DeregisterEventTopic where
  rnf DeregisterEventTopic' {..} =
    Prelude.rnf directoryId `Prelude.seq`
      Prelude.rnf topicName

instance Data.ToHeaders DeregisterEventTopic where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.DeregisterEventTopic" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeregisterEventTopic where
  toJSON DeregisterEventTopic' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Data..= directoryId),
            Prelude.Just ("TopicName" Data..= topicName)
          ]
      )

instance Data.ToPath DeregisterEventTopic where
  toPath = Prelude.const "/"

instance Data.ToQuery DeregisterEventTopic where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a DeregisterEventTopic request.
--
-- /See:/ 'newDeregisterEventTopicResponse' smart constructor.
data DeregisterEventTopicResponse = DeregisterEventTopicResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DeregisterEventTopicResponse where
  rnf DeregisterEventTopicResponse' {..} =
    Prelude.rnf httpStatus
