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
-- Module      : Amazonka.AppConfig.UpdateEnvironment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an environment.
module Amazonka.AppConfig.UpdateEnvironment
  ( -- * Creating a Request
    UpdateEnvironment (..),
    newUpdateEnvironment,

    -- * Request Lenses
    updateEnvironment_description,
    updateEnvironment_monitors,
    updateEnvironment_name,
    updateEnvironment_applicationId,
    updateEnvironment_environmentId,

    -- * Destructuring the Response
    Environment (..),
    newEnvironment,

    -- * Response Lenses
    environment_applicationId,
    environment_description,
    environment_id,
    environment_monitors,
    environment_name,
    environment_state,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEnvironment' smart constructor.
data UpdateEnvironment = UpdateEnvironment'
  { -- | A description of the environment.
    description :: Prelude.Maybe Prelude.Text,
    -- | Amazon CloudWatch alarms to monitor during the deployment process.
    monitors :: Prelude.Maybe [Monitor],
    -- | The name of the environment.
    name :: Prelude.Maybe Prelude.Text,
    -- | The application ID.
    applicationId :: Prelude.Text,
    -- | The environment ID.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateEnvironment_description' - A description of the environment.
--
-- 'monitors', 'updateEnvironment_monitors' - Amazon CloudWatch alarms to monitor during the deployment process.
--
-- 'name', 'updateEnvironment_name' - The name of the environment.
--
-- 'applicationId', 'updateEnvironment_applicationId' - The application ID.
--
-- 'environmentId', 'updateEnvironment_environmentId' - The environment ID.
newUpdateEnvironment ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'environmentId'
  Prelude.Text ->
  UpdateEnvironment
newUpdateEnvironment pApplicationId_ pEnvironmentId_ =
  UpdateEnvironment'
    { description = Prelude.Nothing,
      monitors = Prelude.Nothing,
      name = Prelude.Nothing,
      applicationId = pApplicationId_,
      environmentId = pEnvironmentId_
    }

-- | A description of the environment.
updateEnvironment_description :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_description = Lens.lens (\UpdateEnvironment' {description} -> description) (\s@UpdateEnvironment' {} a -> s {description = a} :: UpdateEnvironment)

-- | Amazon CloudWatch alarms to monitor during the deployment process.
updateEnvironment_monitors :: Lens.Lens' UpdateEnvironment (Prelude.Maybe [Monitor])
updateEnvironment_monitors = Lens.lens (\UpdateEnvironment' {monitors} -> monitors) (\s@UpdateEnvironment' {} a -> s {monitors = a} :: UpdateEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | The name of the environment.
updateEnvironment_name :: Lens.Lens' UpdateEnvironment (Prelude.Maybe Prelude.Text)
updateEnvironment_name = Lens.lens (\UpdateEnvironment' {name} -> name) (\s@UpdateEnvironment' {} a -> s {name = a} :: UpdateEnvironment)

-- | The application ID.
updateEnvironment_applicationId :: Lens.Lens' UpdateEnvironment Prelude.Text
updateEnvironment_applicationId = Lens.lens (\UpdateEnvironment' {applicationId} -> applicationId) (\s@UpdateEnvironment' {} a -> s {applicationId = a} :: UpdateEnvironment)

-- | The environment ID.
updateEnvironment_environmentId :: Lens.Lens' UpdateEnvironment Prelude.Text
updateEnvironment_environmentId = Lens.lens (\UpdateEnvironment' {environmentId} -> environmentId) (\s@UpdateEnvironment' {} a -> s {environmentId = a} :: UpdateEnvironment)

instance Core.AWSRequest UpdateEnvironment where
  type AWSResponse UpdateEnvironment = Environment
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateEnvironment where
  hashWithSalt _salt UpdateEnvironment' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` monitors
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` environmentId

instance Prelude.NFData UpdateEnvironment where
  rnf UpdateEnvironment' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf monitors
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf environmentId

instance Data.ToHeaders UpdateEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateEnvironment where
  toJSON UpdateEnvironment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Monitors" Data..=) Prelude.<$> monitors,
            ("Name" Data..=) Prelude.<$> name
          ]
      )

instance Data.ToPath UpdateEnvironment where
  toPath UpdateEnvironment' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/environments/",
        Data.toBS environmentId
      ]

instance Data.ToQuery UpdateEnvironment where
  toQuery = Prelude.const Prelude.mempty
