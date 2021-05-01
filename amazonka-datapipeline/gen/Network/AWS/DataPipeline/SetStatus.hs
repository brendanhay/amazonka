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
-- Module      : Network.AWS.DataPipeline.SetStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests that the status of the specified physical or logical pipeline
-- objects be updated in the specified pipeline. This update might not
-- occur immediately, but is eventually consistent. The status that can be
-- set depends on the type of object (for example, DataNode or Activity).
-- You cannot perform this operation on @FINISHED@ pipelines and attempting
-- to do so returns @InvalidRequestException@.
module Network.AWS.DataPipeline.SetStatus
  ( -- * Creating a Request
    SetStatus (..),
    newSetStatus,

    -- * Request Lenses
    setStatus_pipelineId,
    setStatus_objectIds,
    setStatus_status,

    -- * Destructuring the Response
    SetStatusResponse (..),
    newSetStatusResponse,
  )
where

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for SetStatus.
--
-- /See:/ 'newSetStatus' smart constructor.
data SetStatus = SetStatus'
  { -- | The ID of the pipeline that contains the objects.
    pipelineId :: Prelude.Text,
    -- | The IDs of the objects. The corresponding objects can be either physical
    -- or components, but not a mix of both types.
    objectIds :: [Prelude.Text],
    -- | The status to be set on all the objects specified in @objectIds@. For
    -- components, use @PAUSE@ or @RESUME@. For instances, use @TRY_CANCEL@,
    -- @RERUN@, or @MARK_FINISHED@.
    status :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineId', 'setStatus_pipelineId' - The ID of the pipeline that contains the objects.
--
-- 'objectIds', 'setStatus_objectIds' - The IDs of the objects. The corresponding objects can be either physical
-- or components, but not a mix of both types.
--
-- 'status', 'setStatus_status' - The status to be set on all the objects specified in @objectIds@. For
-- components, use @PAUSE@ or @RESUME@. For instances, use @TRY_CANCEL@,
-- @RERUN@, or @MARK_FINISHED@.
newSetStatus ::
  -- | 'pipelineId'
  Prelude.Text ->
  -- | 'status'
  Prelude.Text ->
  SetStatus
newSetStatus pPipelineId_ pStatus_ =
  SetStatus'
    { pipelineId = pPipelineId_,
      objectIds = Prelude.mempty,
      status = pStatus_
    }

-- | The ID of the pipeline that contains the objects.
setStatus_pipelineId :: Lens.Lens' SetStatus Prelude.Text
setStatus_pipelineId = Lens.lens (\SetStatus' {pipelineId} -> pipelineId) (\s@SetStatus' {} a -> s {pipelineId = a} :: SetStatus)

-- | The IDs of the objects. The corresponding objects can be either physical
-- or components, but not a mix of both types.
setStatus_objectIds :: Lens.Lens' SetStatus [Prelude.Text]
setStatus_objectIds = Lens.lens (\SetStatus' {objectIds} -> objectIds) (\s@SetStatus' {} a -> s {objectIds = a} :: SetStatus) Prelude.. Prelude._Coerce

-- | The status to be set on all the objects specified in @objectIds@. For
-- components, use @PAUSE@ or @RESUME@. For instances, use @TRY_CANCEL@,
-- @RERUN@, or @MARK_FINISHED@.
setStatus_status :: Lens.Lens' SetStatus Prelude.Text
setStatus_status = Lens.lens (\SetStatus' {status} -> status) (\s@SetStatus' {} a -> s {status = a} :: SetStatus)

instance Prelude.AWSRequest SetStatus where
  type Rs SetStatus = SetStatusResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull SetStatusResponse'

instance Prelude.Hashable SetStatus

instance Prelude.NFData SetStatus

instance Prelude.ToHeaders SetStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("DataPipeline.SetStatus" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON SetStatus where
  toJSON SetStatus' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("pipelineId" Prelude..= pipelineId),
            Prelude.Just ("objectIds" Prelude..= objectIds),
            Prelude.Just ("status" Prelude..= status)
          ]
      )

instance Prelude.ToPath SetStatus where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SetStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetStatusResponse' smart constructor.
data SetStatusResponse = SetStatusResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetStatusResponse ::
  SetStatusResponse
newSetStatusResponse = SetStatusResponse'

instance Prelude.NFData SetStatusResponse
