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
-- Module      : Amazonka.MediaTailor.DeleteLiveSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific live source in a specific source location.
module Amazonka.MediaTailor.DeleteLiveSource
  ( -- * Creating a Request
    DeleteLiveSource (..),
    newDeleteLiveSource,

    -- * Request Lenses
    deleteLiveSource_sourceLocationName,
    deleteLiveSource_liveSourceName,

    -- * Destructuring the Response
    DeleteLiveSourceResponse (..),
    newDeleteLiveSourceResponse,

    -- * Response Lenses
    deleteLiveSourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLiveSource' smart constructor.
data DeleteLiveSource = DeleteLiveSource'
  { -- | The identifier for the source location you are working on.
    sourceLocationName :: Prelude.Text,
    -- | The identifier for the live source you are working on.
    liveSourceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLiveSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceLocationName', 'deleteLiveSource_sourceLocationName' - The identifier for the source location you are working on.
--
-- 'liveSourceName', 'deleteLiveSource_liveSourceName' - The identifier for the live source you are working on.
newDeleteLiveSource ::
  -- | 'sourceLocationName'
  Prelude.Text ->
  -- | 'liveSourceName'
  Prelude.Text ->
  DeleteLiveSource
newDeleteLiveSource
  pSourceLocationName_
  pLiveSourceName_ =
    DeleteLiveSource'
      { sourceLocationName =
          pSourceLocationName_,
        liveSourceName = pLiveSourceName_
      }

-- | The identifier for the source location you are working on.
deleteLiveSource_sourceLocationName :: Lens.Lens' DeleteLiveSource Prelude.Text
deleteLiveSource_sourceLocationName = Lens.lens (\DeleteLiveSource' {sourceLocationName} -> sourceLocationName) (\s@DeleteLiveSource' {} a -> s {sourceLocationName = a} :: DeleteLiveSource)

-- | The identifier for the live source you are working on.
deleteLiveSource_liveSourceName :: Lens.Lens' DeleteLiveSource Prelude.Text
deleteLiveSource_liveSourceName = Lens.lens (\DeleteLiveSource' {liveSourceName} -> liveSourceName) (\s@DeleteLiveSource' {} a -> s {liveSourceName = a} :: DeleteLiveSource)

instance Core.AWSRequest DeleteLiveSource where
  type
    AWSResponse DeleteLiveSource =
      DeleteLiveSourceResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLiveSourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLiveSource where
  hashWithSalt _salt DeleteLiveSource' {..} =
    _salt `Prelude.hashWithSalt` sourceLocationName
      `Prelude.hashWithSalt` liveSourceName

instance Prelude.NFData DeleteLiveSource where
  rnf DeleteLiveSource' {..} =
    Prelude.rnf sourceLocationName
      `Prelude.seq` Prelude.rnf liveSourceName

instance Core.ToHeaders DeleteLiveSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteLiveSource where
  toPath DeleteLiveSource' {..} =
    Prelude.mconcat
      [ "/sourceLocation/",
        Core.toBS sourceLocationName,
        "/liveSource/",
        Core.toBS liveSourceName
      ]

instance Core.ToQuery DeleteLiveSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLiveSourceResponse' smart constructor.
data DeleteLiveSourceResponse = DeleteLiveSourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLiveSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteLiveSourceResponse_httpStatus' - The response's http status code.
newDeleteLiveSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLiveSourceResponse
newDeleteLiveSourceResponse pHttpStatus_ =
  DeleteLiveSourceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteLiveSourceResponse_httpStatus :: Lens.Lens' DeleteLiveSourceResponse Prelude.Int
deleteLiveSourceResponse_httpStatus = Lens.lens (\DeleteLiveSourceResponse' {httpStatus} -> httpStatus) (\s@DeleteLiveSourceResponse' {} a -> s {httpStatus = a} :: DeleteLiveSourceResponse)

instance Prelude.NFData DeleteLiveSourceResponse where
  rnf DeleteLiveSourceResponse' {..} =
    Prelude.rnf httpStatus
