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
-- Module      : Amazonka.DirectoryService.UpdateNumberOfDomainControllers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or removes domain controllers to or from the directory. Based on
-- the difference between current value and new value (provided through
-- this API call), domain controllers will be added or removed. It may take
-- up to 45 minutes for any new domain controllers to become fully active
-- once the requested number of domain controllers is updated. During this
-- time, you cannot make another update request.
module Amazonka.DirectoryService.UpdateNumberOfDomainControllers
  ( -- * Creating a Request
    UpdateNumberOfDomainControllers (..),
    newUpdateNumberOfDomainControllers,

    -- * Request Lenses
    updateNumberOfDomainControllers_directoryId,
    updateNumberOfDomainControllers_desiredNumber,

    -- * Destructuring the Response
    UpdateNumberOfDomainControllersResponse (..),
    newUpdateNumberOfDomainControllersResponse,

    -- * Response Lenses
    updateNumberOfDomainControllersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateNumberOfDomainControllers' smart constructor.
data UpdateNumberOfDomainControllers = UpdateNumberOfDomainControllers'
  { -- | Identifier of the directory to which the domain controllers will be
    -- added or removed.
    directoryId :: Prelude.Text,
    -- | The number of domain controllers desired in the directory.
    desiredNumber :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNumberOfDomainControllers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'updateNumberOfDomainControllers_directoryId' - Identifier of the directory to which the domain controllers will be
-- added or removed.
--
-- 'desiredNumber', 'updateNumberOfDomainControllers_desiredNumber' - The number of domain controllers desired in the directory.
newUpdateNumberOfDomainControllers ::
  -- | 'directoryId'
  Prelude.Text ->
  -- | 'desiredNumber'
  Prelude.Natural ->
  UpdateNumberOfDomainControllers
newUpdateNumberOfDomainControllers
  pDirectoryId_
  pDesiredNumber_ =
    UpdateNumberOfDomainControllers'
      { directoryId =
          pDirectoryId_,
        desiredNumber = pDesiredNumber_
      }

-- | Identifier of the directory to which the domain controllers will be
-- added or removed.
updateNumberOfDomainControllers_directoryId :: Lens.Lens' UpdateNumberOfDomainControllers Prelude.Text
updateNumberOfDomainControllers_directoryId = Lens.lens (\UpdateNumberOfDomainControllers' {directoryId} -> directoryId) (\s@UpdateNumberOfDomainControllers' {} a -> s {directoryId = a} :: UpdateNumberOfDomainControllers)

-- | The number of domain controllers desired in the directory.
updateNumberOfDomainControllers_desiredNumber :: Lens.Lens' UpdateNumberOfDomainControllers Prelude.Natural
updateNumberOfDomainControllers_desiredNumber = Lens.lens (\UpdateNumberOfDomainControllers' {desiredNumber} -> desiredNumber) (\s@UpdateNumberOfDomainControllers' {} a -> s {desiredNumber = a} :: UpdateNumberOfDomainControllers)

instance
  Core.AWSRequest
    UpdateNumberOfDomainControllers
  where
  type
    AWSResponse UpdateNumberOfDomainControllers =
      UpdateNumberOfDomainControllersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateNumberOfDomainControllersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateNumberOfDomainControllers
  where
  hashWithSalt
    _salt
    UpdateNumberOfDomainControllers' {..} =
      _salt `Prelude.hashWithSalt` directoryId
        `Prelude.hashWithSalt` desiredNumber

instance
  Prelude.NFData
    UpdateNumberOfDomainControllers
  where
  rnf UpdateNumberOfDomainControllers' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf desiredNumber

instance
  Core.ToHeaders
    UpdateNumberOfDomainControllers
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.UpdateNumberOfDomainControllers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateNumberOfDomainControllers where
  toJSON UpdateNumberOfDomainControllers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Core..= directoryId),
            Prelude.Just
              ("DesiredNumber" Core..= desiredNumber)
          ]
      )

instance Core.ToPath UpdateNumberOfDomainControllers where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateNumberOfDomainControllers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateNumberOfDomainControllersResponse' smart constructor.
data UpdateNumberOfDomainControllersResponse = UpdateNumberOfDomainControllersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNumberOfDomainControllersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateNumberOfDomainControllersResponse_httpStatus' - The response's http status code.
newUpdateNumberOfDomainControllersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateNumberOfDomainControllersResponse
newUpdateNumberOfDomainControllersResponse
  pHttpStatus_ =
    UpdateNumberOfDomainControllersResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateNumberOfDomainControllersResponse_httpStatus :: Lens.Lens' UpdateNumberOfDomainControllersResponse Prelude.Int
updateNumberOfDomainControllersResponse_httpStatus = Lens.lens (\UpdateNumberOfDomainControllersResponse' {httpStatus} -> httpStatus) (\s@UpdateNumberOfDomainControllersResponse' {} a -> s {httpStatus = a} :: UpdateNumberOfDomainControllersResponse)

instance
  Prelude.NFData
    UpdateNumberOfDomainControllersResponse
  where
  rnf UpdateNumberOfDomainControllersResponse' {..} =
    Prelude.rnf httpStatus
