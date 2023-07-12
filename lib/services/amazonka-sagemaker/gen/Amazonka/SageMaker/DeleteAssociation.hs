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
-- Module      : Amazonka.SageMaker.DeleteAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an association.
module Amazonka.SageMaker.DeleteAssociation
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteAssociation' smart constructor.
data DeleteAssociation = DeleteAssociation'
  { -- | The ARN of the source.
    sourceArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the destination.
    destinationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteAssociation where
  type
    AWSResponse DeleteAssociation =
      DeleteAssociationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAssociationResponse'
            Prelude.<$> (x Data..?> "DestinationArn")
            Prelude.<*> (x Data..?> "SourceArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAssociation where
  hashWithSalt _salt DeleteAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` destinationArn

instance Prelude.NFData DeleteAssociation where
  rnf DeleteAssociation' {..} =
    Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf destinationArn

instance Data.ToHeaders DeleteAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DeleteAssociation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAssociation where
  toJSON DeleteAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SourceArn" Data..= sourceArn),
            Prelude.Just
              ("DestinationArn" Data..= destinationArn)
          ]
      )

instance Data.ToPath DeleteAssociation where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAssociation where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DeleteAssociationResponse where
  rnf DeleteAssociationResponse' {..} =
    Prelude.rnf destinationArn
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf httpStatus
