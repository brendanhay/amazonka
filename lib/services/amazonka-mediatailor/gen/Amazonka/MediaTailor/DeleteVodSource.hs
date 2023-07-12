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
-- Module      : Amazonka.MediaTailor.DeleteVodSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The video on demand (VOD) source to delete.
module Amazonka.MediaTailor.DeleteVodSource
  ( -- * Creating a Request
    DeleteVodSource (..),
    newDeleteVodSource,

    -- * Request Lenses
    deleteVodSource_sourceLocationName,
    deleteVodSource_vodSourceName,

    -- * Destructuring the Response
    DeleteVodSourceResponse (..),
    newDeleteVodSourceResponse,

    -- * Response Lenses
    deleteVodSourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteVodSource' smart constructor.
data DeleteVodSource = DeleteVodSource'
  { -- | The name of the source location associated with this VOD Source.
    sourceLocationName :: Prelude.Text,
    -- | The name of the VOD source.
    vodSourceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVodSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceLocationName', 'deleteVodSource_sourceLocationName' - The name of the source location associated with this VOD Source.
--
-- 'vodSourceName', 'deleteVodSource_vodSourceName' - The name of the VOD source.
newDeleteVodSource ::
  -- | 'sourceLocationName'
  Prelude.Text ->
  -- | 'vodSourceName'
  Prelude.Text ->
  DeleteVodSource
newDeleteVodSource
  pSourceLocationName_
  pVodSourceName_ =
    DeleteVodSource'
      { sourceLocationName =
          pSourceLocationName_,
        vodSourceName = pVodSourceName_
      }

-- | The name of the source location associated with this VOD Source.
deleteVodSource_sourceLocationName :: Lens.Lens' DeleteVodSource Prelude.Text
deleteVodSource_sourceLocationName = Lens.lens (\DeleteVodSource' {sourceLocationName} -> sourceLocationName) (\s@DeleteVodSource' {} a -> s {sourceLocationName = a} :: DeleteVodSource)

-- | The name of the VOD source.
deleteVodSource_vodSourceName :: Lens.Lens' DeleteVodSource Prelude.Text
deleteVodSource_vodSourceName = Lens.lens (\DeleteVodSource' {vodSourceName} -> vodSourceName) (\s@DeleteVodSource' {} a -> s {vodSourceName = a} :: DeleteVodSource)

instance Core.AWSRequest DeleteVodSource where
  type
    AWSResponse DeleteVodSource =
      DeleteVodSourceResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteVodSourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteVodSource where
  hashWithSalt _salt DeleteVodSource' {..} =
    _salt
      `Prelude.hashWithSalt` sourceLocationName
      `Prelude.hashWithSalt` vodSourceName

instance Prelude.NFData DeleteVodSource where
  rnf DeleteVodSource' {..} =
    Prelude.rnf sourceLocationName
      `Prelude.seq` Prelude.rnf vodSourceName

instance Data.ToHeaders DeleteVodSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteVodSource where
  toPath DeleteVodSource' {..} =
    Prelude.mconcat
      [ "/sourceLocation/",
        Data.toBS sourceLocationName,
        "/vodSource/",
        Data.toBS vodSourceName
      ]

instance Data.ToQuery DeleteVodSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteVodSourceResponse' smart constructor.
data DeleteVodSourceResponse = DeleteVodSourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteVodSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteVodSourceResponse_httpStatus' - The response's http status code.
newDeleteVodSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteVodSourceResponse
newDeleteVodSourceResponse pHttpStatus_ =
  DeleteVodSourceResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteVodSourceResponse_httpStatus :: Lens.Lens' DeleteVodSourceResponse Prelude.Int
deleteVodSourceResponse_httpStatus = Lens.lens (\DeleteVodSourceResponse' {httpStatus} -> httpStatus) (\s@DeleteVodSourceResponse' {} a -> s {httpStatus = a} :: DeleteVodSourceResponse)

instance Prelude.NFData DeleteVodSourceResponse where
  rnf DeleteVodSourceResponse' {..} =
    Prelude.rnf httpStatus
