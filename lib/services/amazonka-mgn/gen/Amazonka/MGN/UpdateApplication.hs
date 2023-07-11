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
-- Module      : Amazonka.MGN.UpdateApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update application.
module Amazonka.MGN.UpdateApplication
  ( -- * Creating a Request
    UpdateApplication (..),
    newUpdateApplication,

    -- * Request Lenses
    updateApplication_description,
    updateApplication_name,
    updateApplication_applicationID,

    -- * Destructuring the Response
    Application (..),
    newApplication,

    -- * Response Lenses
    application_applicationAggregatedStatus,
    application_applicationID,
    application_arn,
    application_creationDateTime,
    application_description,
    application_isArchived,
    application_lastModifiedDateTime,
    application_name,
    application_tags,
    application_waveID,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { -- | Application description.
    description :: Prelude.Maybe Prelude.Text,
    -- | Application name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Application ID.
    applicationID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateApplication_description' - Application description.
--
-- 'name', 'updateApplication_name' - Application name.
--
-- 'applicationID', 'updateApplication_applicationID' - Application ID.
newUpdateApplication ::
  -- | 'applicationID'
  Prelude.Text ->
  UpdateApplication
newUpdateApplication pApplicationID_ =
  UpdateApplication'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      applicationID = pApplicationID_
    }

-- | Application description.
updateApplication_description :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_description = Lens.lens (\UpdateApplication' {description} -> description) (\s@UpdateApplication' {} a -> s {description = a} :: UpdateApplication)

-- | Application name.
updateApplication_name :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_name = Lens.lens (\UpdateApplication' {name} -> name) (\s@UpdateApplication' {} a -> s {name = a} :: UpdateApplication)

-- | Application ID.
updateApplication_applicationID :: Lens.Lens' UpdateApplication Prelude.Text
updateApplication_applicationID = Lens.lens (\UpdateApplication' {applicationID} -> applicationID) (\s@UpdateApplication' {} a -> s {applicationID = a} :: UpdateApplication)

instance Core.AWSRequest UpdateApplication where
  type AWSResponse UpdateApplication = Application
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateApplication where
  hashWithSalt _salt UpdateApplication' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` applicationID

instance Prelude.NFData UpdateApplication where
  rnf UpdateApplication' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf applicationID

instance Data.ToHeaders UpdateApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateApplication where
  toJSON UpdateApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("name" Data..=) Prelude.<$> name,
            Prelude.Just
              ("applicationID" Data..= applicationID)
          ]
      )

instance Data.ToPath UpdateApplication where
  toPath = Prelude.const "/UpdateApplication"

instance Data.ToQuery UpdateApplication where
  toQuery = Prelude.const Prelude.mempty
