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
-- Module      : Network.AWS.SageMaker.DeleteAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an association.
module Network.AWS.SageMaker.DeleteAssociation
  ( -- * Creating a Request
    DeleteAssociation (..),
    newDeleteAssociation,

    -- * Request Lenses
    deleteAssociation_sourceArn,
    deleteAssociation_destinationArn,

    -- * Destructuring the Response
    DeleteAssociationResponse (..),
    newDeleteAssociationResponse,

    -- * Response Lenses
    deleteAssociationResponse_destinationArn,
    deleteAssociationResponse_sourceArn,
    deleteAssociationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteAssociation' smart constructor.
data DeleteAssociation = DeleteAssociation'
  { -- | The ARN of the source.
    sourceArn :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the destination.
    destinationArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceArn', 'deleteAssociation_sourceArn' - The ARN of the source.
--
-- 'destinationArn', 'deleteAssociation_destinationArn' - The Amazon Resource Name (ARN) of the destination.
newDeleteAssociation ::
  -- | 'sourceArn'
  Core.Text ->
  -- | 'destinationArn'
  Core.Text ->
  DeleteAssociation
newDeleteAssociation pSourceArn_ pDestinationArn_ =
  DeleteAssociation'
    { sourceArn = pSourceArn_,
      destinationArn = pDestinationArn_
    }

-- | The ARN of the source.
deleteAssociation_sourceArn :: Lens.Lens' DeleteAssociation Core.Text
deleteAssociation_sourceArn = Lens.lens (\DeleteAssociation' {sourceArn} -> sourceArn) (\s@DeleteAssociation' {} a -> s {sourceArn = a} :: DeleteAssociation)

-- | The Amazon Resource Name (ARN) of the destination.
deleteAssociation_destinationArn :: Lens.Lens' DeleteAssociation Core.Text
deleteAssociation_destinationArn = Lens.lens (\DeleteAssociation' {destinationArn} -> destinationArn) (\s@DeleteAssociation' {} a -> s {destinationArn = a} :: DeleteAssociation)

instance Core.AWSRequest DeleteAssociation where
  type
    AWSResponse DeleteAssociation =
      DeleteAssociationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAssociationResponse'
            Core.<$> (x Core..?> "DestinationArn")
            Core.<*> (x Core..?> "SourceArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteAssociation

instance Core.NFData DeleteAssociation

instance Core.ToHeaders DeleteAssociation where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DeleteAssociation" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteAssociation where
  toJSON DeleteAssociation' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SourceArn" Core..= sourceArn),
            Core.Just ("DestinationArn" Core..= destinationArn)
          ]
      )

instance Core.ToPath DeleteAssociation where
  toPath = Core.const "/"

instance Core.ToQuery DeleteAssociation where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteAssociationResponse' smart constructor.
data DeleteAssociationResponse = DeleteAssociationResponse'
  { -- | The Amazon Resource Name (ARN) of the destination.
    destinationArn :: Core.Maybe Core.Text,
    -- | The ARN of the source.
    sourceArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationArn', 'deleteAssociationResponse_destinationArn' - The Amazon Resource Name (ARN) of the destination.
--
-- 'sourceArn', 'deleteAssociationResponse_sourceArn' - The ARN of the source.
--
-- 'httpStatus', 'deleteAssociationResponse_httpStatus' - The response's http status code.
newDeleteAssociationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteAssociationResponse
newDeleteAssociationResponse pHttpStatus_ =
  DeleteAssociationResponse'
    { destinationArn =
        Core.Nothing,
      sourceArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the destination.
deleteAssociationResponse_destinationArn :: Lens.Lens' DeleteAssociationResponse (Core.Maybe Core.Text)
deleteAssociationResponse_destinationArn = Lens.lens (\DeleteAssociationResponse' {destinationArn} -> destinationArn) (\s@DeleteAssociationResponse' {} a -> s {destinationArn = a} :: DeleteAssociationResponse)

-- | The ARN of the source.
deleteAssociationResponse_sourceArn :: Lens.Lens' DeleteAssociationResponse (Core.Maybe Core.Text)
deleteAssociationResponse_sourceArn = Lens.lens (\DeleteAssociationResponse' {sourceArn} -> sourceArn) (\s@DeleteAssociationResponse' {} a -> s {sourceArn = a} :: DeleteAssociationResponse)

-- | The response's http status code.
deleteAssociationResponse_httpStatus :: Lens.Lens' DeleteAssociationResponse Core.Int
deleteAssociationResponse_httpStatus = Lens.lens (\DeleteAssociationResponse' {httpStatus} -> httpStatus) (\s@DeleteAssociationResponse' {} a -> s {httpStatus = a} :: DeleteAssociationResponse)

instance Core.NFData DeleteAssociationResponse
