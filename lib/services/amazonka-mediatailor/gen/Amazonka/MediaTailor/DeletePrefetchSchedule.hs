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
-- Module      : Amazonka.MediaTailor.DeletePrefetchSchedule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a prefetch schedule for a specific playback configuration. If
-- you call @DeletePrefetchSchedule@ on an expired prefetch schedule,
-- MediaTailor returns an HTTP 404 status code. For more information about
-- ad prefetching, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/prefetching-ads.html Using ad prefetching>
-- in the /MediaTailor User Guide/.
module Amazonka.MediaTailor.DeletePrefetchSchedule
  ( -- * Creating a Request
    DeletePrefetchSchedule (..),
    newDeletePrefetchSchedule,

    -- * Request Lenses
    deletePrefetchSchedule_name,
    deletePrefetchSchedule_playbackConfigurationName,

    -- * Destructuring the Response
    DeletePrefetchScheduleResponse (..),
    newDeletePrefetchScheduleResponse,

    -- * Response Lenses
    deletePrefetchScheduleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePrefetchSchedule' smart constructor.
data DeletePrefetchSchedule = DeletePrefetchSchedule'
  { -- | The name of the prefetch schedule. If the action is successful, the
    -- service sends back an HTTP 204 response with an empty HTTP body.
    name :: Prelude.Text,
    -- | The name of the playback configuration for this prefetch schedule.
    playbackConfigurationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePrefetchSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deletePrefetchSchedule_name' - The name of the prefetch schedule. If the action is successful, the
-- service sends back an HTTP 204 response with an empty HTTP body.
--
-- 'playbackConfigurationName', 'deletePrefetchSchedule_playbackConfigurationName' - The name of the playback configuration for this prefetch schedule.
newDeletePrefetchSchedule ::
  -- | 'name'
  Prelude.Text ->
  -- | 'playbackConfigurationName'
  Prelude.Text ->
  DeletePrefetchSchedule
newDeletePrefetchSchedule
  pName_
  pPlaybackConfigurationName_ =
    DeletePrefetchSchedule'
      { name = pName_,
        playbackConfigurationName =
          pPlaybackConfigurationName_
      }

-- | The name of the prefetch schedule. If the action is successful, the
-- service sends back an HTTP 204 response with an empty HTTP body.
deletePrefetchSchedule_name :: Lens.Lens' DeletePrefetchSchedule Prelude.Text
deletePrefetchSchedule_name = Lens.lens (\DeletePrefetchSchedule' {name} -> name) (\s@DeletePrefetchSchedule' {} a -> s {name = a} :: DeletePrefetchSchedule)

-- | The name of the playback configuration for this prefetch schedule.
deletePrefetchSchedule_playbackConfigurationName :: Lens.Lens' DeletePrefetchSchedule Prelude.Text
deletePrefetchSchedule_playbackConfigurationName = Lens.lens (\DeletePrefetchSchedule' {playbackConfigurationName} -> playbackConfigurationName) (\s@DeletePrefetchSchedule' {} a -> s {playbackConfigurationName = a} :: DeletePrefetchSchedule)

instance Core.AWSRequest DeletePrefetchSchedule where
  type
    AWSResponse DeletePrefetchSchedule =
      DeletePrefetchScheduleResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePrefetchScheduleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePrefetchSchedule where
  hashWithSalt _salt DeletePrefetchSchedule' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` playbackConfigurationName

instance Prelude.NFData DeletePrefetchSchedule where
  rnf DeletePrefetchSchedule' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf playbackConfigurationName

instance Core.ToHeaders DeletePrefetchSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeletePrefetchSchedule where
  toPath DeletePrefetchSchedule' {..} =
    Prelude.mconcat
      [ "/prefetchSchedule/",
        Core.toBS playbackConfigurationName,
        "/",
        Core.toBS name
      ]

instance Core.ToQuery DeletePrefetchSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePrefetchScheduleResponse' smart constructor.
data DeletePrefetchScheduleResponse = DeletePrefetchScheduleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePrefetchScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deletePrefetchScheduleResponse_httpStatus' - The response's http status code.
newDeletePrefetchScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePrefetchScheduleResponse
newDeletePrefetchScheduleResponse pHttpStatus_ =
  DeletePrefetchScheduleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deletePrefetchScheduleResponse_httpStatus :: Lens.Lens' DeletePrefetchScheduleResponse Prelude.Int
deletePrefetchScheduleResponse_httpStatus = Lens.lens (\DeletePrefetchScheduleResponse' {httpStatus} -> httpStatus) (\s@DeletePrefetchScheduleResponse' {} a -> s {httpStatus = a} :: DeletePrefetchScheduleResponse)

instance
  Prelude.NFData
    DeletePrefetchScheduleResponse
  where
  rnf DeletePrefetchScheduleResponse' {..} =
    Prelude.rnf httpStatus
