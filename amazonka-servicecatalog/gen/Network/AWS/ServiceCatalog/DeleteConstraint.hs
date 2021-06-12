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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
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
    acceptLanguage :: Core.Maybe Core.Text,
    -- | The identifier of the constraint.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteConstraint
newDeleteConstraint pId_ =
  DeleteConstraint'
    { acceptLanguage = Core.Nothing,
      id = pId_
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
deleteConstraint_acceptLanguage :: Lens.Lens' DeleteConstraint (Core.Maybe Core.Text)
deleteConstraint_acceptLanguage = Lens.lens (\DeleteConstraint' {acceptLanguage} -> acceptLanguage) (\s@DeleteConstraint' {} a -> s {acceptLanguage = a} :: DeleteConstraint)

-- | The identifier of the constraint.
deleteConstraint_id :: Lens.Lens' DeleteConstraint Core.Text
deleteConstraint_id = Lens.lens (\DeleteConstraint' {id} -> id) (\s@DeleteConstraint' {} a -> s {id = a} :: DeleteConstraint)

instance Core.AWSRequest DeleteConstraint where
  type
    AWSResponse DeleteConstraint =
      DeleteConstraintResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteConstraintResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteConstraint

instance Core.NFData DeleteConstraint

instance Core.ToHeaders DeleteConstraint where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DeleteConstraint" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteConstraint where
  toJSON DeleteConstraint' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            Core.Just ("Id" Core..= id)
          ]
      )

instance Core.ToPath DeleteConstraint where
  toPath = Core.const "/"

instance Core.ToQuery DeleteConstraint where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteConstraintResponse' smart constructor.
data DeleteConstraintResponse = DeleteConstraintResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteConstraintResponse
newDeleteConstraintResponse pHttpStatus_ =
  DeleteConstraintResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteConstraintResponse_httpStatus :: Lens.Lens' DeleteConstraintResponse Core.Int
deleteConstraintResponse_httpStatus = Lens.lens (\DeleteConstraintResponse' {httpStatus} -> httpStatus) (\s@DeleteConstraintResponse' {} a -> s {httpStatus = a} :: DeleteConstraintResponse)

instance Core.NFData DeleteConstraintResponse
