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
-- Module      : Amazonka.MGN.ArchiveApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Archive application.
module Amazonka.MGN.ArchiveApplication
  ( -- * Creating a Request
    ArchiveApplication (..),
    newArchiveApplication,

    -- * Request Lenses
    archiveApplication_applicationID,

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

-- | /See:/ 'newArchiveApplication' smart constructor.
data ArchiveApplication = ArchiveApplication'
  { -- | Application ID.
    applicationID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArchiveApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationID', 'archiveApplication_applicationID' - Application ID.
newArchiveApplication ::
  -- | 'applicationID'
  Prelude.Text ->
  ArchiveApplication
newArchiveApplication pApplicationID_ =
  ArchiveApplication'
    { applicationID =
        pApplicationID_
    }

-- | Application ID.
archiveApplication_applicationID :: Lens.Lens' ArchiveApplication Prelude.Text
archiveApplication_applicationID = Lens.lens (\ArchiveApplication' {applicationID} -> applicationID) (\s@ArchiveApplication' {} a -> s {applicationID = a} :: ArchiveApplication)

instance Core.AWSRequest ArchiveApplication where
  type AWSResponse ArchiveApplication = Application
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable ArchiveApplication where
  hashWithSalt _salt ArchiveApplication' {..} =
    _salt `Prelude.hashWithSalt` applicationID

instance Prelude.NFData ArchiveApplication where
  rnf ArchiveApplication' {..} =
    Prelude.rnf applicationID

instance Data.ToHeaders ArchiveApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ArchiveApplication where
  toJSON ArchiveApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("applicationID" Data..= applicationID)
          ]
      )

instance Data.ToPath ArchiveApplication where
  toPath = Prelude.const "/ArchiveApplication"

instance Data.ToQuery ArchiveApplication where
  toQuery = Prelude.const Prelude.mempty
