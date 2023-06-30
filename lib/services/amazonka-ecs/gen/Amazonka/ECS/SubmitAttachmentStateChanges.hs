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
-- Module      : Amazonka.ECS.SubmitAttachmentStateChanges
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action is only used by the Amazon ECS agent, and it is not intended
-- for use outside of the agent.
--
-- Sent to acknowledge that an attachment changed states.
module Amazonka.ECS.SubmitAttachmentStateChanges
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSubmitAttachmentStateChanges' smart constructor.
data SubmitAttachmentStateChanges = SubmitAttachmentStateChanges'
  { -- | The short name or full ARN of the cluster that hosts the container
    -- instance the attachment belongs to.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | Any attachments associated with the state change request.
    attachments :: [AttachmentStateChange]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      attachments = Prelude.mempty
    }

-- | The short name or full ARN of the cluster that hosts the container
-- instance the attachment belongs to.
submitAttachmentStateChanges_cluster :: Lens.Lens' SubmitAttachmentStateChanges (Prelude.Maybe Prelude.Text)
submitAttachmentStateChanges_cluster = Lens.lens (\SubmitAttachmentStateChanges' {cluster} -> cluster) (\s@SubmitAttachmentStateChanges' {} a -> s {cluster = a} :: SubmitAttachmentStateChanges)

-- | Any attachments associated with the state change request.
submitAttachmentStateChanges_attachments :: Lens.Lens' SubmitAttachmentStateChanges [AttachmentStateChange]
submitAttachmentStateChanges_attachments = Lens.lens (\SubmitAttachmentStateChanges' {attachments} -> attachments) (\s@SubmitAttachmentStateChanges' {} a -> s {attachments = a} :: SubmitAttachmentStateChanges) Prelude.. Lens.coerced

instance Core.AWSRequest SubmitAttachmentStateChanges where
  type
    AWSResponse SubmitAttachmentStateChanges =
      SubmitAttachmentStateChangesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SubmitAttachmentStateChangesResponse'
            Prelude.<$> (x Data..?> "acknowledgment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    SubmitAttachmentStateChanges
  where
  hashWithSalt _salt SubmitAttachmentStateChanges' {..} =
    _salt
      `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` attachments

instance Prelude.NFData SubmitAttachmentStateChanges where
  rnf SubmitAttachmentStateChanges' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf attachments

instance Data.ToHeaders SubmitAttachmentStateChanges where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.SubmitAttachmentStateChanges" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SubmitAttachmentStateChanges where
  toJSON SubmitAttachmentStateChanges' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cluster" Data..=) Prelude.<$> cluster,
            Prelude.Just ("attachments" Data..= attachments)
          ]
      )

instance Data.ToPath SubmitAttachmentStateChanges where
  toPath = Prelude.const "/"

instance Data.ToQuery SubmitAttachmentStateChanges where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSubmitAttachmentStateChangesResponse' smart constructor.
data SubmitAttachmentStateChangesResponse = SubmitAttachmentStateChangesResponse'
  { -- | Acknowledgement of the state change.
    acknowledgment :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  SubmitAttachmentStateChangesResponse
newSubmitAttachmentStateChangesResponse pHttpStatus_ =
  SubmitAttachmentStateChangesResponse'
    { acknowledgment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Acknowledgement of the state change.
submitAttachmentStateChangesResponse_acknowledgment :: Lens.Lens' SubmitAttachmentStateChangesResponse (Prelude.Maybe Prelude.Text)
submitAttachmentStateChangesResponse_acknowledgment = Lens.lens (\SubmitAttachmentStateChangesResponse' {acknowledgment} -> acknowledgment) (\s@SubmitAttachmentStateChangesResponse' {} a -> s {acknowledgment = a} :: SubmitAttachmentStateChangesResponse)

-- | The response's http status code.
submitAttachmentStateChangesResponse_httpStatus :: Lens.Lens' SubmitAttachmentStateChangesResponse Prelude.Int
submitAttachmentStateChangesResponse_httpStatus = Lens.lens (\SubmitAttachmentStateChangesResponse' {httpStatus} -> httpStatus) (\s@SubmitAttachmentStateChangesResponse' {} a -> s {httpStatus = a} :: SubmitAttachmentStateChangesResponse)

instance
  Prelude.NFData
    SubmitAttachmentStateChangesResponse
  where
  rnf SubmitAttachmentStateChangesResponse' {..} =
    Prelude.rnf acknowledgment
      `Prelude.seq` Prelude.rnf httpStatus
