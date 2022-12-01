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
-- Module      : Amazonka.ServiceCatalog.DeleteServiceAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a self-service action.
module Amazonka.ServiceCatalog.DeleteServiceAction
  ( -- * Creating a Request
    DeleteServiceAction (..),
    newDeleteServiceAction,

    -- * Request Lenses
    deleteServiceAction_acceptLanguage,
    deleteServiceAction_id,

    -- * Destructuring the Response
    DeleteServiceActionResponse (..),
    newDeleteServiceActionResponse,

    -- * Response Lenses
    deleteServiceActionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newDeleteServiceAction' smart constructor.
data DeleteServiceAction = DeleteServiceAction'
  { -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The self-service action identifier. For example, @act-fs7abcd89wxyz@.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServiceAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'deleteServiceAction_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'id', 'deleteServiceAction_id' - The self-service action identifier. For example, @act-fs7abcd89wxyz@.
newDeleteServiceAction ::
  -- | 'id'
  Prelude.Text ->
  DeleteServiceAction
newDeleteServiceAction pId_ =
  DeleteServiceAction'
    { acceptLanguage =
        Prelude.Nothing,
      id = pId_
    }

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
deleteServiceAction_acceptLanguage :: Lens.Lens' DeleteServiceAction (Prelude.Maybe Prelude.Text)
deleteServiceAction_acceptLanguage = Lens.lens (\DeleteServiceAction' {acceptLanguage} -> acceptLanguage) (\s@DeleteServiceAction' {} a -> s {acceptLanguage = a} :: DeleteServiceAction)

-- | The self-service action identifier. For example, @act-fs7abcd89wxyz@.
deleteServiceAction_id :: Lens.Lens' DeleteServiceAction Prelude.Text
deleteServiceAction_id = Lens.lens (\DeleteServiceAction' {id} -> id) (\s@DeleteServiceAction' {} a -> s {id = a} :: DeleteServiceAction)

instance Core.AWSRequest DeleteServiceAction where
  type
    AWSResponse DeleteServiceAction =
      DeleteServiceActionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteServiceActionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteServiceAction where
  hashWithSalt _salt DeleteServiceAction' {..} =
    _salt `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteServiceAction where
  rnf DeleteServiceAction' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf id

instance Core.ToHeaders DeleteServiceAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DeleteServiceAction" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteServiceAction where
  toJSON DeleteServiceAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("Id" Core..= id)
          ]
      )

instance Core.ToPath DeleteServiceAction where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteServiceAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteServiceActionResponse' smart constructor.
data DeleteServiceActionResponse = DeleteServiceActionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServiceActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteServiceActionResponse_httpStatus' - The response's http status code.
newDeleteServiceActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteServiceActionResponse
newDeleteServiceActionResponse pHttpStatus_ =
  DeleteServiceActionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteServiceActionResponse_httpStatus :: Lens.Lens' DeleteServiceActionResponse Prelude.Int
deleteServiceActionResponse_httpStatus = Lens.lens (\DeleteServiceActionResponse' {httpStatus} -> httpStatus) (\s@DeleteServiceActionResponse' {} a -> s {httpStatus = a} :: DeleteServiceActionResponse)

instance Prelude.NFData DeleteServiceActionResponse where
  rnf DeleteServiceActionResponse' {..} =
    Prelude.rnf httpStatus
