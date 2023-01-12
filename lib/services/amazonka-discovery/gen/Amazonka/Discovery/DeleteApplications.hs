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
-- Module      : Amazonka.Discovery.DeleteApplications
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a list of applications and their associations with configuration
-- items.
module Amazonka.Discovery.DeleteApplications
  ( -- * Creating a Request
    DeleteApplications (..),
    newDeleteApplications,

    -- * Request Lenses
    deleteApplications_configurationIds,

    -- * Destructuring the Response
    DeleteApplicationsResponse (..),
    newDeleteApplicationsResponse,

    -- * Response Lenses
    deleteApplicationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Discovery.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteApplications' smart constructor.
data DeleteApplications = DeleteApplications'
  { -- | Configuration ID of an application to be deleted.
    configurationIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationIds', 'deleteApplications_configurationIds' - Configuration ID of an application to be deleted.
newDeleteApplications ::
  DeleteApplications
newDeleteApplications =
  DeleteApplications'
    { configurationIds =
        Prelude.mempty
    }

-- | Configuration ID of an application to be deleted.
deleteApplications_configurationIds :: Lens.Lens' DeleteApplications [Prelude.Text]
deleteApplications_configurationIds = Lens.lens (\DeleteApplications' {configurationIds} -> configurationIds) (\s@DeleteApplications' {} a -> s {configurationIds = a} :: DeleteApplications) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteApplications where
  type
    AWSResponse DeleteApplications =
      DeleteApplicationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteApplicationsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteApplications where
  hashWithSalt _salt DeleteApplications' {..} =
    _salt `Prelude.hashWithSalt` configurationIds

instance Prelude.NFData DeleteApplications where
  rnf DeleteApplications' {..} =
    Prelude.rnf configurationIds

instance Data.ToHeaders DeleteApplications where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSPoseidonService_V2015_11_01.DeleteApplications" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteApplications where
  toJSON DeleteApplications' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("configurationIds" Data..= configurationIds)
          ]
      )

instance Data.ToPath DeleteApplications where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteApplications where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApplicationsResponse' smart constructor.
data DeleteApplicationsResponse = DeleteApplicationsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplicationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteApplicationsResponse_httpStatus' - The response's http status code.
newDeleteApplicationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteApplicationsResponse
newDeleteApplicationsResponse pHttpStatus_ =
  DeleteApplicationsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteApplicationsResponse_httpStatus :: Lens.Lens' DeleteApplicationsResponse Prelude.Int
deleteApplicationsResponse_httpStatus = Lens.lens (\DeleteApplicationsResponse' {httpStatus} -> httpStatus) (\s@DeleteApplicationsResponse' {} a -> s {httpStatus = a} :: DeleteApplicationsResponse)

instance Prelude.NFData DeleteApplicationsResponse where
  rnf DeleteApplicationsResponse' {..} =
    Prelude.rnf httpStatus
