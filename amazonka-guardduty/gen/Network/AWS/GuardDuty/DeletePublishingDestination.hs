{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.GuardDuty.DeletePublishingDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the publishing definition with the specified @destinationId@.
module Network.AWS.GuardDuty.DeletePublishingDestination
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

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeletePublishingDestination' smart constructor.
data DeletePublishingDestination = DeletePublishingDestination'
  { -- | The unique ID of the detector associated with the publishing destination
    -- to delete.
    detectorId :: Prelude.Text,
    -- | The ID of the publishing destination to delete.
    destinationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.AWSRequest
    DeletePublishingDestination
  where
  type
    Rs DeletePublishingDestination =
      DeletePublishingDestinationResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePublishingDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePublishingDestination

instance Prelude.NFData DeletePublishingDestination

instance
  Prelude.ToHeaders
    DeletePublishingDestination
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath DeletePublishingDestination where
  toPath DeletePublishingDestination' {..} =
    Prelude.mconcat
      [ "/detector/",
        Prelude.toBS detectorId,
        "/publishingDestination/",
        Prelude.toBS destinationId
      ]

instance Prelude.ToQuery DeletePublishingDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePublishingDestinationResponse' smart constructor.
data DeletePublishingDestinationResponse = DeletePublishingDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
