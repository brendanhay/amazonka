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
-- Module      : Amazonka.AppConfig.DeleteEnvironment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an environment. Deleting an environment does not delete a
-- configuration from a host.
module Amazonka.AppConfig.DeleteEnvironment
  ( -- * Creating a Request
    DeleteEnvironment (..),
    newDeleteEnvironment,

    -- * Request Lenses
    deleteEnvironment_applicationId,
    deleteEnvironment_environmentId,

    -- * Destructuring the Response
    DeleteEnvironmentResponse (..),
    newDeleteEnvironmentResponse,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteEnvironment' smart constructor.
data DeleteEnvironment = DeleteEnvironment'
  { -- | The application ID that includes the environment that you want to
    -- delete.
    applicationId :: Prelude.Text,
    -- | The ID of the environment that you want to delete.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteEnvironment_applicationId' - The application ID that includes the environment that you want to
-- delete.
--
-- 'environmentId', 'deleteEnvironment_environmentId' - The ID of the environment that you want to delete.
newDeleteEnvironment ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'environmentId'
  Prelude.Text ->
  DeleteEnvironment
newDeleteEnvironment pApplicationId_ pEnvironmentId_ =
  DeleteEnvironment'
    { applicationId = pApplicationId_,
      environmentId = pEnvironmentId_
    }

-- | The application ID that includes the environment that you want to
-- delete.
deleteEnvironment_applicationId :: Lens.Lens' DeleteEnvironment Prelude.Text
deleteEnvironment_applicationId = Lens.lens (\DeleteEnvironment' {applicationId} -> applicationId) (\s@DeleteEnvironment' {} a -> s {applicationId = a} :: DeleteEnvironment)

-- | The ID of the environment that you want to delete.
deleteEnvironment_environmentId :: Lens.Lens' DeleteEnvironment Prelude.Text
deleteEnvironment_environmentId = Lens.lens (\DeleteEnvironment' {environmentId} -> environmentId) (\s@DeleteEnvironment' {} a -> s {environmentId = a} :: DeleteEnvironment)

instance Core.AWSRequest DeleteEnvironment where
  type
    AWSResponse DeleteEnvironment =
      DeleteEnvironmentResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteEnvironmentResponse'

instance Prelude.Hashable DeleteEnvironment where
  hashWithSalt _salt DeleteEnvironment' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` environmentId

instance Prelude.NFData DeleteEnvironment where
  rnf DeleteEnvironment' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf environmentId

instance Core.ToHeaders DeleteEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteEnvironment where
  toPath DeleteEnvironment' {..} =
    Prelude.mconcat
      [ "/applications/",
        Core.toBS applicationId,
        "/environments/",
        Core.toBS environmentId
      ]

instance Core.ToQuery DeleteEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEnvironmentResponse' smart constructor.
data DeleteEnvironmentResponse = DeleteEnvironmentResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteEnvironmentResponse ::
  DeleteEnvironmentResponse
newDeleteEnvironmentResponse =
  DeleteEnvironmentResponse'

instance Prelude.NFData DeleteEnvironmentResponse where
  rnf _ = ()
