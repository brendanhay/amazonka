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
-- Module      : Network.AWS.ServiceCatalog.DeleteConstraint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified constraint.
--
-- A delegated admin is authorized to invoke this command.
module Network.AWS.ServiceCatalog.DeleteConstraint
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'newDeleteConstraint' smart constructor.
data DeleteConstraint = DeleteConstraint'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the constraint.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- -   @en@ - English (default)
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
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
deleteConstraint_acceptLanguage :: Lens.Lens' DeleteConstraint (Prelude.Maybe Prelude.Text)
deleteConstraint_acceptLanguage = Lens.lens (\DeleteConstraint' {acceptLanguage} -> acceptLanguage) (\s@DeleteConstraint' {} a -> s {acceptLanguage = a} :: DeleteConstraint)

-- | The identifier of the constraint.
deleteConstraint_id :: Lens.Lens' DeleteConstraint Prelude.Text
deleteConstraint_id = Lens.lens (\DeleteConstraint' {id} -> id) (\s@DeleteConstraint' {} a -> s {id = a} :: DeleteConstraint)

instance Prelude.AWSRequest DeleteConstraint where
  type Rs DeleteConstraint = DeleteConstraintResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteConstraintResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteConstraint

instance Prelude.NFData DeleteConstraint

instance Prelude.ToHeaders DeleteConstraint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWS242ServiceCatalogService.DeleteConstraint" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteConstraint where
  toJSON DeleteConstraint' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Prelude..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("Id" Prelude..= id)
          ]
      )

instance Prelude.ToPath DeleteConstraint where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteConstraint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConstraintResponse' smart constructor.
data DeleteConstraintResponse = DeleteConstraintResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteConstraintResponse
