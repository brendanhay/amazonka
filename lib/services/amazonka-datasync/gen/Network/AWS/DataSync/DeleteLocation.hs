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
-- Module      : Network.AWS.DataSync.DeleteLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the configuration of a location used by DataSync.
module Network.AWS.DataSync.DeleteLocation
  ( -- * Creating a Request
    DeleteLocation (..),
    newDeleteLocation,

    -- * Request Lenses
    deleteLocation_locationArn,

    -- * Destructuring the Response
    DeleteLocationResponse (..),
    newDeleteLocationResponse,

    -- * Response Lenses
    deleteLocationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DataSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | DeleteLocation
--
-- /See:/ 'newDeleteLocation' smart constructor.
data DeleteLocation = DeleteLocation'
  { -- | The Amazon Resource Name (ARN) of the location to delete.
    locationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationArn', 'deleteLocation_locationArn' - The Amazon Resource Name (ARN) of the location to delete.
newDeleteLocation ::
  -- | 'locationArn'
  Prelude.Text ->
  DeleteLocation
newDeleteLocation pLocationArn_ =
  DeleteLocation' {locationArn = pLocationArn_}

-- | The Amazon Resource Name (ARN) of the location to delete.
deleteLocation_locationArn :: Lens.Lens' DeleteLocation Prelude.Text
deleteLocation_locationArn = Lens.lens (\DeleteLocation' {locationArn} -> locationArn) (\s@DeleteLocation' {} a -> s {locationArn = a} :: DeleteLocation)

instance Core.AWSRequest DeleteLocation where
  type
    AWSResponse DeleteLocation =
      DeleteLocationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLocationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLocation

instance Prelude.NFData DeleteLocation

instance Core.ToHeaders DeleteLocation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("FmrsService.DeleteLocation" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteLocation where
  toJSON DeleteLocation' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("LocationArn" Core..= locationArn)]
      )

instance Core.ToPath DeleteLocation where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteLocation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLocationResponse' smart constructor.
data DeleteLocationResponse = DeleteLocationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLocationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteLocationResponse_httpStatus' - The response's http status code.
newDeleteLocationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLocationResponse
newDeleteLocationResponse pHttpStatus_ =
  DeleteLocationResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteLocationResponse_httpStatus :: Lens.Lens' DeleteLocationResponse Prelude.Int
deleteLocationResponse_httpStatus = Lens.lens (\DeleteLocationResponse' {httpStatus} -> httpStatus) (\s@DeleteLocationResponse' {} a -> s {httpStatus = a} :: DeleteLocationResponse)

instance Prelude.NFData DeleteLocationResponse
