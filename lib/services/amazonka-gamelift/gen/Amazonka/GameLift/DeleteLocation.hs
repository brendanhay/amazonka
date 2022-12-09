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
-- Module      : Amazonka.GameLift.DeleteLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a custom location.
--
-- Before deleting a custom location, review any fleets currently using the
-- custom location and deregister the location if it is in use. For more
-- information see,
-- <https://docs.aws.amazon.com/gamelift/latest/apireference/API_DeregisterCompute.html DeregisterCompute>.
module Amazonka.GameLift.DeleteLocation
  ( -- * Creating a Request
    DeleteLocation (..),
    newDeleteLocation,

    -- * Request Lenses
    deleteLocation_locationName,

    -- * Destructuring the Response
    DeleteLocationResponse (..),
    newDeleteLocationResponse,

    -- * Response Lenses
    deleteLocationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLocation' smart constructor.
data DeleteLocation = DeleteLocation'
  { -- | The location name of the custom location to be deleted.
    locationName :: Prelude.Text
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
-- 'locationName', 'deleteLocation_locationName' - The location name of the custom location to be deleted.
newDeleteLocation ::
  -- | 'locationName'
  Prelude.Text ->
  DeleteLocation
newDeleteLocation pLocationName_ =
  DeleteLocation' {locationName = pLocationName_}

-- | The location name of the custom location to be deleted.
deleteLocation_locationName :: Lens.Lens' DeleteLocation Prelude.Text
deleteLocation_locationName = Lens.lens (\DeleteLocation' {locationName} -> locationName) (\s@DeleteLocation' {} a -> s {locationName = a} :: DeleteLocation)

instance Core.AWSRequest DeleteLocation where
  type
    AWSResponse DeleteLocation =
      DeleteLocationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLocationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLocation where
  hashWithSalt _salt DeleteLocation' {..} =
    _salt `Prelude.hashWithSalt` locationName

instance Prelude.NFData DeleteLocation where
  rnf DeleteLocation' {..} = Prelude.rnf locationName

instance Data.ToHeaders DeleteLocation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.DeleteLocation" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteLocation where
  toJSON DeleteLocation' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("LocationName" Data..= locationName)]
      )

instance Data.ToPath DeleteLocation where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteLocation where
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

instance Prelude.NFData DeleteLocationResponse where
  rnf DeleteLocationResponse' {..} =
    Prelude.rnf httpStatus
