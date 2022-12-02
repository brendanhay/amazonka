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
-- Module      : Amazonka.Location.DeleteMap
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a map resource from your AWS account.
--
-- This operation deletes the resource permanently. If the map is being
-- used in an application, the map may not render.
module Amazonka.Location.DeleteMap
  ( -- * Creating a Request
    DeleteMap (..),
    newDeleteMap,

    -- * Request Lenses
    deleteMap_mapName,

    -- * Destructuring the Response
    DeleteMapResponse (..),
    newDeleteMapResponse,

    -- * Response Lenses
    deleteMapResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteMap' smart constructor.
data DeleteMap = DeleteMap'
  { -- | The name of the map resource to be deleted.
    mapName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMap' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mapName', 'deleteMap_mapName' - The name of the map resource to be deleted.
newDeleteMap ::
  -- | 'mapName'
  Prelude.Text ->
  DeleteMap
newDeleteMap pMapName_ =
  DeleteMap' {mapName = pMapName_}

-- | The name of the map resource to be deleted.
deleteMap_mapName :: Lens.Lens' DeleteMap Prelude.Text
deleteMap_mapName = Lens.lens (\DeleteMap' {mapName} -> mapName) (\s@DeleteMap' {} a -> s {mapName = a} :: DeleteMap)

instance Core.AWSRequest DeleteMap where
  type AWSResponse DeleteMap = DeleteMapResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteMapResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteMap where
  hashWithSalt _salt DeleteMap' {..} =
    _salt `Prelude.hashWithSalt` mapName

instance Prelude.NFData DeleteMap where
  rnf DeleteMap' {..} = Prelude.rnf mapName

instance Data.ToHeaders DeleteMap where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteMap where
  toPath DeleteMap' {..} =
    Prelude.mconcat
      ["/maps/v0/maps/", Data.toBS mapName]

instance Data.ToQuery DeleteMap where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteMapResponse' smart constructor.
data DeleteMapResponse = DeleteMapResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMapResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteMapResponse_httpStatus' - The response's http status code.
newDeleteMapResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteMapResponse
newDeleteMapResponse pHttpStatus_ =
  DeleteMapResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteMapResponse_httpStatus :: Lens.Lens' DeleteMapResponse Prelude.Int
deleteMapResponse_httpStatus = Lens.lens (\DeleteMapResponse' {httpStatus} -> httpStatus) (\s@DeleteMapResponse' {} a -> s {httpStatus = a} :: DeleteMapResponse)

instance Prelude.NFData DeleteMapResponse where
  rnf DeleteMapResponse' {..} = Prelude.rnf httpStatus
