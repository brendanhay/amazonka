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
-- Module      : Network.AWS.ECS.SubmitAttachmentStateChanges
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action is only used by the Amazon ECS agent, and it is not intended
-- for use outside of the agent.
--
-- Sent to acknowledge that an attachment changed states.
module Network.AWS.ECS.SubmitAttachmentStateChanges
  ( -- * Creating a Request
    SubmitAttachmentStateChanges (..),
    newSubmitAttachmentStateChanges,

    -- * Request Lenses
    submitAttachmentStateChanges_cluster,
    submitAttachmentStateChanges_attachments,

    -- * Destructuring the Response
    SubmitAttachmentStateChangesResponse (..),
    newSubmitAttachmentStateChangesResponse,

    -- * Response Lenses
    submitAttachmentStateChangesResponse_acknowledgment,
    submitAttachmentStateChangesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSubmitAttachmentStateChanges' smart constructor.
data SubmitAttachmentStateChanges = SubmitAttachmentStateChanges'
  { -- | The short name or full ARN of the cluster that hosts the container
    -- instance the attachment belongs to.
    cluster :: Core.Maybe Core.Text,
    -- | Any attachments associated with the state change request.
    attachments :: [AttachmentStateChange]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SubmitAttachmentStateChanges' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'submitAttachmentStateChanges_cluster' - The short name or full ARN of the cluster that hosts the container
-- instance the attachment belongs to.
--
-- 'attachments', 'submitAttachmentStateChanges_attachments' - Any attachments associated with the state change request.
newSubmitAttachmentStateChanges ::
  SubmitAttachmentStateChanges
newSubmitAttachmentStateChanges =
  SubmitAttachmentStateChanges'
    { cluster =
        Core.Nothing,
      attachments = Core.mempty
    }

-- | The short name or full ARN of the cluster that hosts the container
-- instance the attachment belongs to.
submitAttachmentStateChanges_cluster :: Lens.Lens' SubmitAttachmentStateChanges (Core.Maybe Core.Text)
submitAttachmentStateChanges_cluster = Lens.lens (\SubmitAttachmentStateChanges' {cluster} -> cluster) (\s@SubmitAttachmentStateChanges' {} a -> s {cluster = a} :: SubmitAttachmentStateChanges)

-- | Any attachments associated with the state change request.
submitAttachmentStateChanges_attachments :: Lens.Lens' SubmitAttachmentStateChanges [AttachmentStateChange]
submitAttachmentStateChanges_attachments = Lens.lens (\SubmitAttachmentStateChanges' {attachments} -> attachments) (\s@SubmitAttachmentStateChanges' {} a -> s {attachments = a} :: SubmitAttachmentStateChanges) Core.. Lens._Coerce

instance Core.AWSRequest SubmitAttachmentStateChanges where
  type
    AWSResponse SubmitAttachmentStateChanges =
      SubmitAttachmentStateChangesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SubmitAttachmentStateChangesResponse'
            Core.<$> (x Core..?> "acknowledgment")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SubmitAttachmentStateChanges

instance Core.NFData SubmitAttachmentStateChanges

instance Core.ToHeaders SubmitAttachmentStateChanges where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.SubmitAttachmentStateChanges" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SubmitAttachmentStateChanges where
  toJSON SubmitAttachmentStateChanges' {..} =
    Core.object
      ( Core.catMaybes
          [ ("cluster" Core..=) Core.<$> cluster,
            Core.Just ("attachments" Core..= attachments)
          ]
      )

instance Core.ToPath SubmitAttachmentStateChanges where
  toPath = Core.const "/"

instance Core.ToQuery SubmitAttachmentStateChanges where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSubmitAttachmentStateChangesResponse' smart constructor.
data SubmitAttachmentStateChangesResponse = SubmitAttachmentStateChangesResponse'
  { -- | Acknowledgement of the state change.
    acknowledgment :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SubmitAttachmentStateChangesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acknowledgment', 'submitAttachmentStateChangesResponse_acknowledgment' - Acknowledgement of the state change.
--
-- 'httpStatus', 'submitAttachmentStateChangesResponse_httpStatus' - The response's http status code.
newSubmitAttachmentStateChangesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SubmitAttachmentStateChangesResponse
newSubmitAttachmentStateChangesResponse pHttpStatus_ =
  SubmitAttachmentStateChangesResponse'
    { acknowledgment =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Acknowledgement of the state change.
submitAttachmentStateChangesResponse_acknowledgment :: Lens.Lens' SubmitAttachmentStateChangesResponse (Core.Maybe Core.Text)
submitAttachmentStateChangesResponse_acknowledgment = Lens.lens (\SubmitAttachmentStateChangesResponse' {acknowledgment} -> acknowledgment) (\s@SubmitAttachmentStateChangesResponse' {} a -> s {acknowledgment = a} :: SubmitAttachmentStateChangesResponse)

-- | The response's http status code.
submitAttachmentStateChangesResponse_httpStatus :: Lens.Lens' SubmitAttachmentStateChangesResponse Core.Int
submitAttachmentStateChangesResponse_httpStatus = Lens.lens (\SubmitAttachmentStateChangesResponse' {httpStatus} -> httpStatus) (\s@SubmitAttachmentStateChangesResponse' {} a -> s {httpStatus = a} :: SubmitAttachmentStateChangesResponse)

instance
  Core.NFData
    SubmitAttachmentStateChangesResponse
