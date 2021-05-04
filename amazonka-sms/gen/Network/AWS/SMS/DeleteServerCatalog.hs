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
-- Module      : Network.AWS.SMS.DeleteServerCatalog
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all servers from your server catalog.
module Network.AWS.SMS.DeleteServerCatalog
  ( -- * Creating a Request
    DeleteServerCatalog (..),
    newDeleteServerCatalog,

    -- * Destructuring the Response
    DeleteServerCatalogResponse (..),
    newDeleteServerCatalogResponse,

    -- * Response Lenses
    deleteServerCatalogResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newDeleteServerCatalog' smart constructor.
data DeleteServerCatalog = DeleteServerCatalog'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteServerCatalog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteServerCatalog ::
  DeleteServerCatalog
newDeleteServerCatalog = DeleteServerCatalog'

instance Prelude.AWSRequest DeleteServerCatalog where
  type
    Rs DeleteServerCatalog =
      DeleteServerCatalogResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteServerCatalogResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteServerCatalog

instance Prelude.NFData DeleteServerCatalog

instance Prelude.ToHeaders DeleteServerCatalog where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSServerMigrationService_V2016_10_24.DeleteServerCatalog" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteServerCatalog where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath DeleteServerCatalog where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteServerCatalog where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteServerCatalogResponse' smart constructor.
data DeleteServerCatalogResponse = DeleteServerCatalogResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteServerCatalogResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteServerCatalogResponse_httpStatus' - The response's http status code.
newDeleteServerCatalogResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteServerCatalogResponse
newDeleteServerCatalogResponse pHttpStatus_ =
  DeleteServerCatalogResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteServerCatalogResponse_httpStatus :: Lens.Lens' DeleteServerCatalogResponse Prelude.Int
deleteServerCatalogResponse_httpStatus = Lens.lens (\DeleteServerCatalogResponse' {httpStatus} -> httpStatus) (\s@DeleteServerCatalogResponse' {} a -> s {httpStatus = a} :: DeleteServerCatalogResponse)

instance Prelude.NFData DeleteServerCatalogResponse
