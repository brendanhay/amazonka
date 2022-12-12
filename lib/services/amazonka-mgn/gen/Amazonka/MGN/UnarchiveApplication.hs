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
-- Module      : Amazonka.MGN.UnarchiveApplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unarchive application.
module Amazonka.MGN.UnarchiveApplication
  ( -- * Creating a Request
    UnarchiveApplication (..),
    newUnarchiveApplication,

    -- * Request Lenses
    unarchiveApplication_applicationID,

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

-- | /See:/ 'newUnarchiveApplication' smart constructor.
data UnarchiveApplication = UnarchiveApplication'
  { -- | Application ID.
    applicationID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnarchiveApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationID', 'unarchiveApplication_applicationID' - Application ID.
newUnarchiveApplication ::
  -- | 'applicationID'
  Prelude.Text ->
  UnarchiveApplication
newUnarchiveApplication pApplicationID_ =
  UnarchiveApplication'
    { applicationID =
        pApplicationID_
    }

-- | Application ID.
unarchiveApplication_applicationID :: Lens.Lens' UnarchiveApplication Prelude.Text
unarchiveApplication_applicationID = Lens.lens (\UnarchiveApplication' {applicationID} -> applicationID) (\s@UnarchiveApplication' {} a -> s {applicationID = a} :: UnarchiveApplication)

instance Core.AWSRequest UnarchiveApplication where
  type AWSResponse UnarchiveApplication = Application
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UnarchiveApplication where
  hashWithSalt _salt UnarchiveApplication' {..} =
    _salt `Prelude.hashWithSalt` applicationID

instance Prelude.NFData UnarchiveApplication where
  rnf UnarchiveApplication' {..} =
    Prelude.rnf applicationID

instance Data.ToHeaders UnarchiveApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UnarchiveApplication where
  toJSON UnarchiveApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("applicationID" Data..= applicationID)
          ]
      )

instance Data.ToPath UnarchiveApplication where
  toPath = Prelude.const "/UnarchiveApplication"

instance Data.ToQuery UnarchiveApplication where
  toQuery = Prelude.const Prelude.mempty
