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
-- Module      : Amazonka.IoTRoboRunner.DeleteSite
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants permission to delete a site
module Amazonka.IoTRoboRunner.DeleteSite
  ( -- * Creating a Request
    DeleteSite (..),
    newDeleteSite,

    -- * Request Lenses
    deleteSite_id,

    -- * Destructuring the Response
    DeleteSiteResponse (..),
    newDeleteSiteResponse,

    -- * Response Lenses
    deleteSiteResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTRoboRunner.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSite' smart constructor.
data DeleteSite = DeleteSite'
  { id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteSite_id' - Undocumented member.
newDeleteSite ::
  -- | 'id'
  Prelude.Text ->
  DeleteSite
newDeleteSite pId_ = DeleteSite' {id = pId_}

-- | Undocumented member.
deleteSite_id :: Lens.Lens' DeleteSite Prelude.Text
deleteSite_id = Lens.lens (\DeleteSite' {id} -> id) (\s@DeleteSite' {} a -> s {id = a} :: DeleteSite)

instance Core.AWSRequest DeleteSite where
  type AWSResponse DeleteSite = DeleteSiteResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSiteResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSite where
  hashWithSalt _salt DeleteSite' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteSite where
  rnf DeleteSite' {..} = Prelude.rnf id

instance Data.ToHeaders DeleteSite where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteSite where
  toJSON DeleteSite' {..} =
    Data.object
      (Prelude.catMaybes [Prelude.Just ("id" Data..= id)])

instance Data.ToPath DeleteSite where
  toPath = Prelude.const "/deleteSite"

instance Data.ToQuery DeleteSite where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSiteResponse' smart constructor.
data DeleteSiteResponse = DeleteSiteResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSiteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSiteResponse_httpStatus' - The response's http status code.
newDeleteSiteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSiteResponse
newDeleteSiteResponse pHttpStatus_ =
  DeleteSiteResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteSiteResponse_httpStatus :: Lens.Lens' DeleteSiteResponse Prelude.Int
deleteSiteResponse_httpStatus = Lens.lens (\DeleteSiteResponse' {httpStatus} -> httpStatus) (\s@DeleteSiteResponse' {} a -> s {httpStatus = a} :: DeleteSiteResponse)

instance Prelude.NFData DeleteSiteResponse where
  rnf DeleteSiteResponse' {..} = Prelude.rnf httpStatus
