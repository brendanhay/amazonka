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
-- Module      : Amazonka.ServiceCatalog.DeleteConstraint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified constraint.
--
-- A delegated admin is authorized to invoke this command.
module Amazonka.ServiceCatalog.DeleteConstraint
  ( -- * Creating a Request
    DeleteConstraint (..),
    newDeleteConstraint,

    -- * Request Lenses
    deleteConstraint_acceptLanguage,
    deleteConstraint_id,

    -- * Destructuring the Response
    DeleteConstraintResponse (..),
    newDeleteConstraintResponse,

    -- * Response Lenses
    deleteConstraintResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newDeleteConstraint' smart constructor.
data DeleteConstraint = DeleteConstraint'
  { -- | The language code.
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the constraint.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConstraint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'deleteConstraint_acceptLanguage' - The language code.
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'id', 'deleteConstraint_id' - The identifier of the constraint.
newDeleteConstraint ::
  -- | 'id'
  Prelude.Text ->
  DeleteConstraint
newDeleteConstraint pId_ =
  DeleteConstraint'
    { acceptLanguage = Prelude.Nothing,
      id = pId_
    }

-- | The language code.
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
deleteConstraint_acceptLanguage :: Lens.Lens' DeleteConstraint (Prelude.Maybe Prelude.Text)
deleteConstraint_acceptLanguage = Lens.lens (\DeleteConstraint' {acceptLanguage} -> acceptLanguage) (\s@DeleteConstraint' {} a -> s {acceptLanguage = a} :: DeleteConstraint)

-- | The identifier of the constraint.
deleteConstraint_id :: Lens.Lens' DeleteConstraint Prelude.Text
deleteConstraint_id = Lens.lens (\DeleteConstraint' {id} -> id) (\s@DeleteConstraint' {} a -> s {id = a} :: DeleteConstraint)

instance Core.AWSRequest DeleteConstraint where
  type
    AWSResponse DeleteConstraint =
      DeleteConstraintResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteConstraintResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteConstraint where
  hashWithSalt _salt DeleteConstraint' {..} =
    _salt
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteConstraint where
  rnf DeleteConstraint' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders DeleteConstraint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.DeleteConstraint" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteConstraint where
  toJSON DeleteConstraint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance Data.ToPath DeleteConstraint where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteConstraint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConstraintResponse' smart constructor.
data DeleteConstraintResponse = DeleteConstraintResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConstraintResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteConstraintResponse_httpStatus' - The response's http status code.
newDeleteConstraintResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteConstraintResponse
newDeleteConstraintResponse pHttpStatus_ =
  DeleteConstraintResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteConstraintResponse_httpStatus :: Lens.Lens' DeleteConstraintResponse Prelude.Int
deleteConstraintResponse_httpStatus = Lens.lens (\DeleteConstraintResponse' {httpStatus} -> httpStatus) (\s@DeleteConstraintResponse' {} a -> s {httpStatus = a} :: DeleteConstraintResponse)

instance Prelude.NFData DeleteConstraintResponse where
  rnf DeleteConstraintResponse' {..} =
    Prelude.rnf httpStatus
