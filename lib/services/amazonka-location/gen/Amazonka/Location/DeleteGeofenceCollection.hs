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
-- Module      : Amazonka.Location.DeleteGeofenceCollection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a geofence collection from your AWS account.
--
-- This operation deletes the resource permanently. If the geofence
-- collection is the target of a tracker resource, the devices will no
-- longer be monitored.
module Amazonka.Location.DeleteGeofenceCollection
  ( -- * Creating a Request
    DeleteGeofenceCollection (..),
    newDeleteGeofenceCollection,

    -- * Request Lenses
    deleteGeofenceCollection_collectionName,

    -- * Destructuring the Response
    DeleteGeofenceCollectionResponse (..),
    newDeleteGeofenceCollectionResponse,

    -- * Response Lenses
    deleteGeofenceCollectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteGeofenceCollection' smart constructor.
data DeleteGeofenceCollection = DeleteGeofenceCollection'
  { -- | The name of the geofence collection to be deleted.
    collectionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGeofenceCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectionName', 'deleteGeofenceCollection_collectionName' - The name of the geofence collection to be deleted.
newDeleteGeofenceCollection ::
  -- | 'collectionName'
  Prelude.Text ->
  DeleteGeofenceCollection
newDeleteGeofenceCollection pCollectionName_ =
  DeleteGeofenceCollection'
    { collectionName =
        pCollectionName_
    }

-- | The name of the geofence collection to be deleted.
deleteGeofenceCollection_collectionName :: Lens.Lens' DeleteGeofenceCollection Prelude.Text
deleteGeofenceCollection_collectionName = Lens.lens (\DeleteGeofenceCollection' {collectionName} -> collectionName) (\s@DeleteGeofenceCollection' {} a -> s {collectionName = a} :: DeleteGeofenceCollection)

instance Core.AWSRequest DeleteGeofenceCollection where
  type
    AWSResponse DeleteGeofenceCollection =
      DeleteGeofenceCollectionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteGeofenceCollectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteGeofenceCollection where
  hashWithSalt _salt DeleteGeofenceCollection' {..} =
    _salt `Prelude.hashWithSalt` collectionName

instance Prelude.NFData DeleteGeofenceCollection where
  rnf DeleteGeofenceCollection' {..} =
    Prelude.rnf collectionName

instance Core.ToHeaders DeleteGeofenceCollection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteGeofenceCollection where
  toPath DeleteGeofenceCollection' {..} =
    Prelude.mconcat
      [ "/geofencing/v0/collections/",
        Core.toBS collectionName
      ]

instance Core.ToQuery DeleteGeofenceCollection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteGeofenceCollectionResponse' smart constructor.
data DeleteGeofenceCollectionResponse = DeleteGeofenceCollectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGeofenceCollectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteGeofenceCollectionResponse_httpStatus' - The response's http status code.
newDeleteGeofenceCollectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteGeofenceCollectionResponse
newDeleteGeofenceCollectionResponse pHttpStatus_ =
  DeleteGeofenceCollectionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteGeofenceCollectionResponse_httpStatus :: Lens.Lens' DeleteGeofenceCollectionResponse Prelude.Int
deleteGeofenceCollectionResponse_httpStatus = Lens.lens (\DeleteGeofenceCollectionResponse' {httpStatus} -> httpStatus) (\s@DeleteGeofenceCollectionResponse' {} a -> s {httpStatus = a} :: DeleteGeofenceCollectionResponse)

instance
  Prelude.NFData
    DeleteGeofenceCollectionResponse
  where
  rnf DeleteGeofenceCollectionResponse' {..} =
    Prelude.rnf httpStatus
