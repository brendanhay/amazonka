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
-- Module      : Amazonka.ChimeSdkMediaPipelines.DeleteMediaPipeline
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the media pipeline.
module Amazonka.ChimeSdkMediaPipelines.DeleteMediaPipeline
  ( -- * Creating a Request
    DeleteMediaPipeline (..),
    newDeleteMediaPipeline,

    -- * Request Lenses
    deleteMediaPipeline_mediaPipelineId,

    -- * Destructuring the Response
    DeleteMediaPipelineResponse (..),
    newDeleteMediaPipelineResponse,
  )
where

import Amazonka.ChimeSdkMediaPipelines.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteMediaPipeline' smart constructor.
data DeleteMediaPipeline = DeleteMediaPipeline'
  { -- | The ID of the media pipeline to delete.
    mediaPipelineId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMediaPipeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaPipelineId', 'deleteMediaPipeline_mediaPipelineId' - The ID of the media pipeline to delete.
newDeleteMediaPipeline ::
  -- | 'mediaPipelineId'
  Prelude.Text ->
  DeleteMediaPipeline
newDeleteMediaPipeline pMediaPipelineId_ =
  DeleteMediaPipeline'
    { mediaPipelineId =
        pMediaPipelineId_
    }

-- | The ID of the media pipeline to delete.
deleteMediaPipeline_mediaPipelineId :: Lens.Lens' DeleteMediaPipeline Prelude.Text
deleteMediaPipeline_mediaPipelineId = Lens.lens (\DeleteMediaPipeline' {mediaPipelineId} -> mediaPipelineId) (\s@DeleteMediaPipeline' {} a -> s {mediaPipelineId = a} :: DeleteMediaPipeline)

instance Core.AWSRequest DeleteMediaPipeline where
  type
    AWSResponse DeleteMediaPipeline =
      DeleteMediaPipelineResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteMediaPipelineResponse'

instance Prelude.Hashable DeleteMediaPipeline where
  hashWithSalt _salt DeleteMediaPipeline' {..} =
    _salt `Prelude.hashWithSalt` mediaPipelineId

instance Prelude.NFData DeleteMediaPipeline where
  rnf DeleteMediaPipeline' {..} =
    Prelude.rnf mediaPipelineId

instance Core.ToHeaders DeleteMediaPipeline where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteMediaPipeline where
  toPath DeleteMediaPipeline' {..} =
    Prelude.mconcat
      ["/sdk-media-pipelines/", Core.toBS mediaPipelineId]

instance Core.ToQuery DeleteMediaPipeline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMediaPipelineResponse' smart constructor.
data DeleteMediaPipelineResponse = DeleteMediaPipelineResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMediaPipelineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteMediaPipelineResponse ::
  DeleteMediaPipelineResponse
newDeleteMediaPipelineResponse =
  DeleteMediaPipelineResponse'

instance Prelude.NFData DeleteMediaPipelineResponse where
  rnf _ = ()
