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
-- Module      : Amazonka.CodeDeploy.DeleteApplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an application.
module Amazonka.CodeDeploy.DeleteApplication
  ( -- * Creating a Request
    DeleteApplication (..),
    newDeleteApplication,

    -- * Request Lenses
    deleteApplication_applicationName,

    -- * Destructuring the Response
    DeleteApplicationResponse (..),
    newDeleteApplicationResponse,
  )
where

import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @DeleteApplication@ operation.
--
-- /See:/ 'newDeleteApplication' smart constructor.
data DeleteApplication = DeleteApplication'
  { -- | The name of an CodeDeploy application associated with the IAM user or
    -- Amazon Web Services account.
    applicationName :: Prelude.Text
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
-- 'applicationName', 'deleteApplication_applicationName' - The name of an CodeDeploy application associated with the IAM user or
-- Amazon Web Services account.
newDeleteApplication ::
  -- | 'applicationName'
  Prelude.Text ->
  DeleteApplication
newDeleteApplication pApplicationName_ =
  DeleteApplication'
    { applicationName =
        pApplicationName_
    }

-- | The name of an CodeDeploy application associated with the IAM user or
-- Amazon Web Services account.
deleteApplication_applicationName :: Lens.Lens' DeleteApplication Prelude.Text
deleteApplication_applicationName = Lens.lens (\DeleteApplication' {applicationName} -> applicationName) (\s@DeleteApplication' {} a -> s {applicationName = a} :: DeleteApplication)

instance Core.AWSRequest DeleteApplication where
  type
    AWSResponse DeleteApplication =
      DeleteApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteApplicationResponse'

instance Prelude.Hashable DeleteApplication where
  hashWithSalt _salt DeleteApplication' {..} =
    _salt `Prelude.hashWithSalt` applicationName

instance Prelude.NFData DeleteApplication where
  rnf DeleteApplication' {..} =
    Prelude.rnf applicationName

instance Data.ToHeaders DeleteApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeDeploy_20141006.DeleteApplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteApplication where
  toJSON DeleteApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("applicationName" Data..= applicationName)
          ]
      )

instance Data.ToPath DeleteApplication where
  toPath = Prelude.const "/"

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
