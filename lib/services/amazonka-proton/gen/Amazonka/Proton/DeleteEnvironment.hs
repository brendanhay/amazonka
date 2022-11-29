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
-- Module      : Amazonka.Proton.DeleteEnvironment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an environment.
module Amazonka.Proton.DeleteEnvironment
  ( -- * Creating a Request
    DeleteEnvironment (..),
    newDeleteEnvironment,

    -- * Request Lenses
    deleteEnvironment_name,

    -- * Destructuring the Response
    DeleteEnvironmentResponse (..),
    newDeleteEnvironmentResponse,

    -- * Response Lenses
    deleteEnvironmentResponse_environment,
    deleteEnvironmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteEnvironment' smart constructor.
data DeleteEnvironment = DeleteEnvironment'
  { -- | The name of the environment to delete.
    name :: Prelude.Text
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
-- 'name', 'deleteEnvironment_name' - The name of the environment to delete.
newDeleteEnvironment ::
  -- | 'name'
  Prelude.Text ->
  DeleteEnvironment
newDeleteEnvironment pName_ =
  DeleteEnvironment' {name = pName_}

-- | The name of the environment to delete.
deleteEnvironment_name :: Lens.Lens' DeleteEnvironment Prelude.Text
deleteEnvironment_name = Lens.lens (\DeleteEnvironment' {name} -> name) (\s@DeleteEnvironment' {} a -> s {name = a} :: DeleteEnvironment)

instance Core.AWSRequest DeleteEnvironment where
  type
    AWSResponse DeleteEnvironment =
      DeleteEnvironmentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteEnvironmentResponse'
            Prelude.<$> (x Core..?> "environment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteEnvironment where
  hashWithSalt _salt DeleteEnvironment' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteEnvironment where
  rnf DeleteEnvironment' {..} = Prelude.rnf name

instance Core.ToHeaders DeleteEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AwsProton20200720.DeleteEnvironment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteEnvironment where
  toJSON DeleteEnvironment' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Core..= name)]
      )

instance Core.ToPath DeleteEnvironment where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEnvironmentResponse' smart constructor.
data DeleteEnvironmentResponse = DeleteEnvironmentResponse'
  { -- | The detailed data of the environment being deleted.
    environment :: Prelude.Maybe Environment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environment', 'deleteEnvironmentResponse_environment' - The detailed data of the environment being deleted.
--
-- 'httpStatus', 'deleteEnvironmentResponse_httpStatus' - The response's http status code.
newDeleteEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteEnvironmentResponse
newDeleteEnvironmentResponse pHttpStatus_ =
  DeleteEnvironmentResponse'
    { environment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The detailed data of the environment being deleted.
deleteEnvironmentResponse_environment :: Lens.Lens' DeleteEnvironmentResponse (Prelude.Maybe Environment)
deleteEnvironmentResponse_environment = Lens.lens (\DeleteEnvironmentResponse' {environment} -> environment) (\s@DeleteEnvironmentResponse' {} a -> s {environment = a} :: DeleteEnvironmentResponse)

-- | The response's http status code.
deleteEnvironmentResponse_httpStatus :: Lens.Lens' DeleteEnvironmentResponse Prelude.Int
deleteEnvironmentResponse_httpStatus = Lens.lens (\DeleteEnvironmentResponse' {httpStatus} -> httpStatus) (\s@DeleteEnvironmentResponse' {} a -> s {httpStatus = a} :: DeleteEnvironmentResponse)

instance Prelude.NFData DeleteEnvironmentResponse where
  rnf DeleteEnvironmentResponse' {..} =
    Prelude.rnf environment
      `Prelude.seq` Prelude.rnf httpStatus
