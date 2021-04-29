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
-- Module      : Network.AWS.ServiceCatalog.DeleteServiceAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a self-service action.
module Network.AWS.ServiceCatalog.DeleteServiceAction
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceCatalog.Types

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteServiceAction where
  type
    Rs DeleteServiceAction =
      DeleteServiceActionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteServiceActionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteServiceAction

instance Prelude.NFData DeleteServiceAction

instance Prelude.ToHeaders DeleteServiceAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWS242ServiceCatalogService.DeleteServiceAction" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteServiceAction where
  toJSON DeleteServiceAction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Prelude..=)
              Prelude.<$> acceptLanguage,
            Prelude.Just ("Id" Prelude..= id)
          ]
      )

instance Prelude.ToPath DeleteServiceAction where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteServiceAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteServiceActionResponse' smart constructor.
data DeleteServiceActionResponse = DeleteServiceActionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteServiceActionResponse
