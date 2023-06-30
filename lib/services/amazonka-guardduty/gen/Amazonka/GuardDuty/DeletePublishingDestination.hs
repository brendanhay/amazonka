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
-- Module      : Amazonka.GuardDuty.DeletePublishingDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the publishing definition with the specified @destinationId@.
module Amazonka.GuardDuty.DeletePublishingDestination
  ( -- * Creating a Request
    DeletePublishingDestination (..),
    newDeletePublishingDestination,

    -- * Request Lenses
    deletePublishingDestination_detectorId,
    deletePublishingDestination_destinationId,

    -- * Destructuring the Response
    DeletePublishingDestinationResponse (..),
    newDeletePublishingDestinationResponse,

    -- * Response Lenses
    deletePublishingDestinationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePublishingDestination' smart constructor.
data DeletePublishingDestination = DeletePublishingDestination'
  { -- | The unique ID of the detector associated with the publishing destination
    -- to delete.
    detectorId :: Prelude.Text,
    -- | The ID of the publishing destination to delete.
    destinationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePublishingDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'deletePublishingDestination_detectorId' - The unique ID of the detector associated with the publishing destination
-- to delete.
--
-- 'destinationId', 'deletePublishingDestination_destinationId' - The ID of the publishing destination to delete.
newDeletePublishingDestination ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'destinationId'
  Prelude.Text ->
  DeletePublishingDestination
newDeletePublishingDestination
  pDetectorId_
  pDestinationId_ =
    DeletePublishingDestination'
      { detectorId =
          pDetectorId_,
        destinationId = pDestinationId_
      }

-- | The unique ID of the detector associated with the publishing destination
-- to delete.
deletePublishingDestination_detectorId :: Lens.Lens' DeletePublishingDestination Prelude.Text
deletePublishingDestination_detectorId = Lens.lens (\DeletePublishingDestination' {detectorId} -> detectorId) (\s@DeletePublishingDestination' {} a -> s {detectorId = a} :: DeletePublishingDestination)

-- | The ID of the publishing destination to delete.
deletePublishingDestination_destinationId :: Lens.Lens' DeletePublishingDestination Prelude.Text
deletePublishingDestination_destinationId = Lens.lens (\DeletePublishingDestination' {destinationId} -> destinationId) (\s@DeletePublishingDestination' {} a -> s {destinationId = a} :: DeletePublishingDestination)

instance Core.AWSRequest DeletePublishingDestination where
  type
    AWSResponse DeletePublishingDestination =
      DeletePublishingDestinationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePublishingDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePublishingDestination where
  hashWithSalt _salt DeletePublishingDestination' {..} =
    _salt
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` destinationId

instance Prelude.NFData DeletePublishingDestination where
  rnf DeletePublishingDestination' {..} =
    Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf destinationId

instance Data.ToHeaders DeletePublishingDestination where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeletePublishingDestination where
  toPath DeletePublishingDestination' {..} =
    Prelude.mconcat
      [ "/detector/",
        Data.toBS detectorId,
        "/publishingDestination/",
        Data.toBS destinationId
      ]

instance Data.ToQuery DeletePublishingDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePublishingDestinationResponse' smart constructor.
data DeletePublishingDestinationResponse = DeletePublishingDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePublishingDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deletePublishingDestinationResponse_httpStatus' - The response's http status code.
newDeletePublishingDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePublishingDestinationResponse
newDeletePublishingDestinationResponse pHttpStatus_ =
  DeletePublishingDestinationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deletePublishingDestinationResponse_httpStatus :: Lens.Lens' DeletePublishingDestinationResponse Prelude.Int
deletePublishingDestinationResponse_httpStatus = Lens.lens (\DeletePublishingDestinationResponse' {httpStatus} -> httpStatus) (\s@DeletePublishingDestinationResponse' {} a -> s {httpStatus = a} :: DeletePublishingDestinationResponse)

instance
  Prelude.NFData
    DeletePublishingDestinationResponse
  where
  rnf DeletePublishingDestinationResponse' {..} =
    Prelude.rnf httpStatus
