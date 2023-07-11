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
-- Module      : Amazonka.MGN.DisassociateApplications
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociate applications from wave.
module Amazonka.MGN.DisassociateApplications
  ( -- * Creating a Request
    DisassociateApplications (..),
    newDisassociateApplications,

    -- * Request Lenses
    disassociateApplications_applicationIDs,
    disassociateApplications_waveID,

    -- * Destructuring the Response
    DisassociateApplicationsResponse (..),
    newDisassociateApplicationsResponse,

    -- * Response Lenses
    disassociateApplicationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateApplications' smart constructor.
data DisassociateApplications = DisassociateApplications'
  { -- | Application IDs list.
    applicationIDs :: Prelude.NonEmpty Prelude.Text,
    -- | Wave ID.
    waveID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateApplications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationIDs', 'disassociateApplications_applicationIDs' - Application IDs list.
--
-- 'waveID', 'disassociateApplications_waveID' - Wave ID.
newDisassociateApplications ::
  -- | 'applicationIDs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'waveID'
  Prelude.Text ->
  DisassociateApplications
newDisassociateApplications pApplicationIDs_ pWaveID_ =
  DisassociateApplications'
    { applicationIDs =
        Lens.coerced Lens.# pApplicationIDs_,
      waveID = pWaveID_
    }

-- | Application IDs list.
disassociateApplications_applicationIDs :: Lens.Lens' DisassociateApplications (Prelude.NonEmpty Prelude.Text)
disassociateApplications_applicationIDs = Lens.lens (\DisassociateApplications' {applicationIDs} -> applicationIDs) (\s@DisassociateApplications' {} a -> s {applicationIDs = a} :: DisassociateApplications) Prelude.. Lens.coerced

-- | Wave ID.
disassociateApplications_waveID :: Lens.Lens' DisassociateApplications Prelude.Text
disassociateApplications_waveID = Lens.lens (\DisassociateApplications' {waveID} -> waveID) (\s@DisassociateApplications' {} a -> s {waveID = a} :: DisassociateApplications)

instance Core.AWSRequest DisassociateApplications where
  type
    AWSResponse DisassociateApplications =
      DisassociateApplicationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateApplicationsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateApplications where
  hashWithSalt _salt DisassociateApplications' {..} =
    _salt
      `Prelude.hashWithSalt` applicationIDs
      `Prelude.hashWithSalt` waveID

instance Prelude.NFData DisassociateApplications where
  rnf DisassociateApplications' {..} =
    Prelude.rnf applicationIDs
      `Prelude.seq` Prelude.rnf waveID

instance Data.ToHeaders DisassociateApplications where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateApplications where
  toJSON DisassociateApplications' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("applicationIDs" Data..= applicationIDs),
            Prelude.Just ("waveID" Data..= waveID)
          ]
      )

instance Data.ToPath DisassociateApplications where
  toPath = Prelude.const "/DisassociateApplications"

instance Data.ToQuery DisassociateApplications where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateApplicationsResponse' smart constructor.
data DisassociateApplicationsResponse = DisassociateApplicationsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateApplicationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateApplicationsResponse_httpStatus' - The response's http status code.
newDisassociateApplicationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateApplicationsResponse
newDisassociateApplicationsResponse pHttpStatus_ =
  DisassociateApplicationsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateApplicationsResponse_httpStatus :: Lens.Lens' DisassociateApplicationsResponse Prelude.Int
disassociateApplicationsResponse_httpStatus = Lens.lens (\DisassociateApplicationsResponse' {httpStatus} -> httpStatus) (\s@DisassociateApplicationsResponse' {} a -> s {httpStatus = a} :: DisassociateApplicationsResponse)

instance
  Prelude.NFData
    DisassociateApplicationsResponse
  where
  rnf DisassociateApplicationsResponse' {..} =
    Prelude.rnf httpStatus
