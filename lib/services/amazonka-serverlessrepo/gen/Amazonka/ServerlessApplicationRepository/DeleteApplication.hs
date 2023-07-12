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
-- Module      : Amazonka.ServerlessApplicationRepository.DeleteApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified application.
module Amazonka.ServerlessApplicationRepository.DeleteApplication
  ( -- * Creating a Request
    DeleteApplication (..),
    newDeleteApplication,

    -- * Request Lenses
    deleteApplication_applicationId,

    -- * Destructuring the Response
    DeleteApplicationResponse (..),
    newDeleteApplicationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServerlessApplicationRepository.Types

-- | /See:/ 'newDeleteApplication' smart constructor.
data DeleteApplication = DeleteApplication'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteApplication_applicationId' - The Amazon Resource Name (ARN) of the application.
newDeleteApplication ::
  -- | 'applicationId'
  Prelude.Text ->
  DeleteApplication
newDeleteApplication pApplicationId_ =
  DeleteApplication' {applicationId = pApplicationId_}

-- | The Amazon Resource Name (ARN) of the application.
deleteApplication_applicationId :: Lens.Lens' DeleteApplication Prelude.Text
deleteApplication_applicationId = Lens.lens (\DeleteApplication' {applicationId} -> applicationId) (\s@DeleteApplication' {} a -> s {applicationId = a} :: DeleteApplication)

instance Core.AWSRequest DeleteApplication where
  type
    AWSResponse DeleteApplication =
      DeleteApplicationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteApplicationResponse'

instance Prelude.Hashable DeleteApplication where
  hashWithSalt _salt DeleteApplication' {..} =
    _salt `Prelude.hashWithSalt` applicationId

instance Prelude.NFData DeleteApplication where
  rnf DeleteApplication' {..} =
    Prelude.rnf applicationId

instance Data.ToHeaders DeleteApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteApplication where
  toPath DeleteApplication' {..} =
    Prelude.mconcat
      ["/applications/", Data.toBS applicationId]

instance Data.ToQuery DeleteApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApplicationResponse' smart constructor.
data DeleteApplicationResponse = DeleteApplicationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteApplicationResponse ::
  DeleteApplicationResponse
newDeleteApplicationResponse =
  DeleteApplicationResponse'

instance Prelude.NFData DeleteApplicationResponse where
  rnf _ = ()
