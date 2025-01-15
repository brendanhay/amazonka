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
-- Module      : Amazonka.MigrationHubReFactorSpaces.DeleteEnvironment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Web Services Migration Hub Refactor Spaces
-- environment. Before you can delete an environment, you must first delete
-- any applications and services within the environment.
module Amazonka.MigrationHubReFactorSpaces.DeleteEnvironment
  ( -- * Creating a Request
    DeleteEnvironment (..),
    newDeleteEnvironment,

    -- * Request Lenses
    deleteEnvironment_environmentIdentifier,

    -- * Destructuring the Response
    DeleteEnvironmentResponse (..),
    newDeleteEnvironmentResponse,

    -- * Response Lenses
    deleteEnvironmentResponse_arn,
    deleteEnvironmentResponse_environmentId,
    deleteEnvironmentResponse_lastUpdatedTime,
    deleteEnvironmentResponse_name,
    deleteEnvironmentResponse_state,
    deleteEnvironmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubReFactorSpaces.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteEnvironment' smart constructor.
data DeleteEnvironment = DeleteEnvironment'
  { -- | The ID of the environment.
    environmentIdentifier :: Prelude.Text
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
-- 'environmentIdentifier', 'deleteEnvironment_environmentIdentifier' - The ID of the environment.
newDeleteEnvironment ::
  -- | 'environmentIdentifier'
  Prelude.Text ->
  DeleteEnvironment
newDeleteEnvironment pEnvironmentIdentifier_ =
  DeleteEnvironment'
    { environmentIdentifier =
        pEnvironmentIdentifier_
    }

-- | The ID of the environment.
deleteEnvironment_environmentIdentifier :: Lens.Lens' DeleteEnvironment Prelude.Text
deleteEnvironment_environmentIdentifier = Lens.lens (\DeleteEnvironment' {environmentIdentifier} -> environmentIdentifier) (\s@DeleteEnvironment' {} a -> s {environmentIdentifier = a} :: DeleteEnvironment)

instance Core.AWSRequest DeleteEnvironment where
  type
    AWSResponse DeleteEnvironment =
      DeleteEnvironmentResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteEnvironmentResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "EnvironmentId")
            Prelude.<*> (x Data..?> "LastUpdatedTime")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteEnvironment where
  hashWithSalt _salt DeleteEnvironment' {..} =
    _salt `Prelude.hashWithSalt` environmentIdentifier

instance Prelude.NFData DeleteEnvironment where
  rnf DeleteEnvironment' {..} =
    Prelude.rnf environmentIdentifier

instance Data.ToHeaders DeleteEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteEnvironment where
  toPath DeleteEnvironment' {..} =
    Prelude.mconcat
      ["/environments/", Data.toBS environmentIdentifier]

instance Data.ToQuery DeleteEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEnvironmentResponse' smart constructor.
data DeleteEnvironmentResponse = DeleteEnvironmentResponse'
  { -- | The Amazon Resource Name (ARN) of the environment.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the environment.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that indicates when the environment was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the environment.
    name :: Prelude.Maybe Prelude.Text,
    -- | The current state of the environment.
    state :: Prelude.Maybe EnvironmentState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteEnvironmentResponse_arn' - The Amazon Resource Name (ARN) of the environment.
--
-- 'environmentId', 'deleteEnvironmentResponse_environmentId' - The unique identifier of the environment.
--
-- 'lastUpdatedTime', 'deleteEnvironmentResponse_lastUpdatedTime' - A timestamp that indicates when the environment was last updated.
--
-- 'name', 'deleteEnvironmentResponse_name' - The name of the environment.
--
-- 'state', 'deleteEnvironmentResponse_state' - The current state of the environment.
--
-- 'httpStatus', 'deleteEnvironmentResponse_httpStatus' - The response's http status code.
newDeleteEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteEnvironmentResponse
newDeleteEnvironmentResponse pHttpStatus_ =
  DeleteEnvironmentResponse'
    { arn = Prelude.Nothing,
      environmentId = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the environment.
deleteEnvironmentResponse_arn :: Lens.Lens' DeleteEnvironmentResponse (Prelude.Maybe Prelude.Text)
deleteEnvironmentResponse_arn = Lens.lens (\DeleteEnvironmentResponse' {arn} -> arn) (\s@DeleteEnvironmentResponse' {} a -> s {arn = a} :: DeleteEnvironmentResponse)

-- | The unique identifier of the environment.
deleteEnvironmentResponse_environmentId :: Lens.Lens' DeleteEnvironmentResponse (Prelude.Maybe Prelude.Text)
deleteEnvironmentResponse_environmentId = Lens.lens (\DeleteEnvironmentResponse' {environmentId} -> environmentId) (\s@DeleteEnvironmentResponse' {} a -> s {environmentId = a} :: DeleteEnvironmentResponse)

-- | A timestamp that indicates when the environment was last updated.
deleteEnvironmentResponse_lastUpdatedTime :: Lens.Lens' DeleteEnvironmentResponse (Prelude.Maybe Prelude.UTCTime)
deleteEnvironmentResponse_lastUpdatedTime = Lens.lens (\DeleteEnvironmentResponse' {lastUpdatedTime} -> lastUpdatedTime) (\s@DeleteEnvironmentResponse' {} a -> s {lastUpdatedTime = a} :: DeleteEnvironmentResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the environment.
deleteEnvironmentResponse_name :: Lens.Lens' DeleteEnvironmentResponse (Prelude.Maybe Prelude.Text)
deleteEnvironmentResponse_name = Lens.lens (\DeleteEnvironmentResponse' {name} -> name) (\s@DeleteEnvironmentResponse' {} a -> s {name = a} :: DeleteEnvironmentResponse)

-- | The current state of the environment.
deleteEnvironmentResponse_state :: Lens.Lens' DeleteEnvironmentResponse (Prelude.Maybe EnvironmentState)
deleteEnvironmentResponse_state = Lens.lens (\DeleteEnvironmentResponse' {state} -> state) (\s@DeleteEnvironmentResponse' {} a -> s {state = a} :: DeleteEnvironmentResponse)

-- | The response's http status code.
deleteEnvironmentResponse_httpStatus :: Lens.Lens' DeleteEnvironmentResponse Prelude.Int
deleteEnvironmentResponse_httpStatus = Lens.lens (\DeleteEnvironmentResponse' {httpStatus} -> httpStatus) (\s@DeleteEnvironmentResponse' {} a -> s {httpStatus = a} :: DeleteEnvironmentResponse)

instance Prelude.NFData DeleteEnvironmentResponse where
  rnf DeleteEnvironmentResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf environmentId `Prelude.seq`
        Prelude.rnf lastUpdatedTime `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf state `Prelude.seq`
              Prelude.rnf httpStatus
