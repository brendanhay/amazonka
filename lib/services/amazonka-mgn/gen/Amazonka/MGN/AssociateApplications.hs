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
-- Module      : Amazonka.MGN.AssociateApplications
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associate applications to wave.
module Amazonka.MGN.AssociateApplications
  ( -- * Creating a Request
    AssociateApplications (..),
    newAssociateApplications,

    -- * Request Lenses
    associateApplications_applicationIDs,
    associateApplications_waveID,

    -- * Destructuring the Response
    AssociateApplicationsResponse (..),
    newAssociateApplicationsResponse,

    -- * Response Lenses
    associateApplicationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateApplications' smart constructor.
data AssociateApplications = AssociateApplications'
  { -- | Application IDs list.
    applicationIDs :: Prelude.NonEmpty Prelude.Text,
    -- | Wave ID.
    waveID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateApplications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationIDs', 'associateApplications_applicationIDs' - Application IDs list.
--
-- 'waveID', 'associateApplications_waveID' - Wave ID.
newAssociateApplications ::
  -- | 'applicationIDs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'waveID'
  Prelude.Text ->
  AssociateApplications
newAssociateApplications pApplicationIDs_ pWaveID_ =
  AssociateApplications'
    { applicationIDs =
        Lens.coerced Lens.# pApplicationIDs_,
      waveID = pWaveID_
    }

-- | Application IDs list.
associateApplications_applicationIDs :: Lens.Lens' AssociateApplications (Prelude.NonEmpty Prelude.Text)
associateApplications_applicationIDs = Lens.lens (\AssociateApplications' {applicationIDs} -> applicationIDs) (\s@AssociateApplications' {} a -> s {applicationIDs = a} :: AssociateApplications) Prelude.. Lens.coerced

-- | Wave ID.
associateApplications_waveID :: Lens.Lens' AssociateApplications Prelude.Text
associateApplications_waveID = Lens.lens (\AssociateApplications' {waveID} -> waveID) (\s@AssociateApplications' {} a -> s {waveID = a} :: AssociateApplications)

instance Core.AWSRequest AssociateApplications where
  type
    AWSResponse AssociateApplications =
      AssociateApplicationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateApplicationsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateApplications where
  hashWithSalt _salt AssociateApplications' {..} =
    _salt
      `Prelude.hashWithSalt` applicationIDs
      `Prelude.hashWithSalt` waveID

instance Prelude.NFData AssociateApplications where
  rnf AssociateApplications' {..} =
    Prelude.rnf applicationIDs
      `Prelude.seq` Prelude.rnf waveID

instance Data.ToHeaders AssociateApplications where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateApplications where
  toJSON AssociateApplications' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("applicationIDs" Data..= applicationIDs),
            Prelude.Just ("waveID" Data..= waveID)
          ]
      )

instance Data.ToPath AssociateApplications where
  toPath = Prelude.const "/AssociateApplications"

instance Data.ToQuery AssociateApplications where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateApplicationsResponse' smart constructor.
data AssociateApplicationsResponse = AssociateApplicationsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateApplicationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateApplicationsResponse_httpStatus' - The response's http status code.
newAssociateApplicationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateApplicationsResponse
newAssociateApplicationsResponse pHttpStatus_ =
  AssociateApplicationsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateApplicationsResponse_httpStatus :: Lens.Lens' AssociateApplicationsResponse Prelude.Int
associateApplicationsResponse_httpStatus = Lens.lens (\AssociateApplicationsResponse' {httpStatus} -> httpStatus) (\s@AssociateApplicationsResponse' {} a -> s {httpStatus = a} :: AssociateApplicationsResponse)

instance Prelude.NFData AssociateApplicationsResponse where
  rnf AssociateApplicationsResponse' {..} =
    Prelude.rnf httpStatus
