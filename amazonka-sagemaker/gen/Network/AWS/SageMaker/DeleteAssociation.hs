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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteAssociation' smart constructor.
data DeleteAssociation = DeleteAssociation'
  { -- | The ARN of the source.
    sourceArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the destination.
    destinationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'destinationArn'
  Prelude.Text ->
  DeleteAssociation
newDeleteAssociation pSourceArn_ pDestinationArn_ =
  DeleteAssociation'
    { sourceArn = pSourceArn_,
      destinationArn = pDestinationArn_
    }

-- | The ARN of the source.
deleteAssociation_sourceArn :: Lens.Lens' DeleteAssociation Prelude.Text
deleteAssociation_sourceArn = Lens.lens (\DeleteAssociation' {sourceArn} -> sourceArn) (\s@DeleteAssociation' {} a -> s {sourceArn = a} :: DeleteAssociation)

-- | The Amazon Resource Name (ARN) of the destination.
deleteAssociation_destinationArn :: Lens.Lens' DeleteAssociation Prelude.Text
deleteAssociation_destinationArn = Lens.lens (\DeleteAssociation' {destinationArn} -> destinationArn) (\s@DeleteAssociation' {} a -> s {destinationArn = a} :: DeleteAssociation)

instance Prelude.AWSRequest DeleteAssociation where
  type Rs DeleteAssociation = DeleteAssociationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAssociationResponse'
            Prelude.<$> (x Prelude..?> "DestinationArn")
            Prelude.<*> (x Prelude..?> "SourceArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAssociation

instance Prelude.NFData DeleteAssociation

instance Prelude.ToHeaders DeleteAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.DeleteAssociation" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteAssociation where
  toJSON DeleteAssociation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SourceArn" Prelude..= sourceArn),
            Prelude.Just
              ("DestinationArn" Prelude..= destinationArn)
          ]
      )

instance Prelude.ToPath DeleteAssociation where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAssociationResponse' smart constructor.
data DeleteAssociationResponse = DeleteAssociationResponse'
  { -- | The Amazon Resource Name (ARN) of the destination.
    destinationArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the source.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteAssociationResponse
newDeleteAssociationResponse pHttpStatus_ =
  DeleteAssociationResponse'
    { destinationArn =
        Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the destination.
deleteAssociationResponse_destinationArn :: Lens.Lens' DeleteAssociationResponse (Prelude.Maybe Prelude.Text)
deleteAssociationResponse_destinationArn = Lens.lens (\DeleteAssociationResponse' {destinationArn} -> destinationArn) (\s@DeleteAssociationResponse' {} a -> s {destinationArn = a} :: DeleteAssociationResponse)

-- | The ARN of the source.
deleteAssociationResponse_sourceArn :: Lens.Lens' DeleteAssociationResponse (Prelude.Maybe Prelude.Text)
deleteAssociationResponse_sourceArn = Lens.lens (\DeleteAssociationResponse' {sourceArn} -> sourceArn) (\s@DeleteAssociationResponse' {} a -> s {sourceArn = a} :: DeleteAssociationResponse)

-- | The response's http status code.
deleteAssociationResponse_httpStatus :: Lens.Lens' DeleteAssociationResponse Prelude.Int
deleteAssociationResponse_httpStatus = Lens.lens (\DeleteAssociationResponse' {httpStatus} -> httpStatus) (\s@DeleteAssociationResponse' {} a -> s {httpStatus = a} :: DeleteAssociationResponse)

instance Prelude.NFData DeleteAssociationResponse
