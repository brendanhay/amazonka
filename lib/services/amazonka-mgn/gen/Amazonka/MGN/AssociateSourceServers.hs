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
-- Module      : Amazonka.MGN.AssociateSourceServers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associate source servers to application.
module Amazonka.MGN.AssociateSourceServers
  ( -- * Creating a Request
    AssociateSourceServers (..),
    newAssociateSourceServers,

    -- * Request Lenses
    associateSourceServers_applicationID,
    associateSourceServers_sourceServerIDs,

    -- * Destructuring the Response
    AssociateSourceServersResponse (..),
    newAssociateSourceServersResponse,

    -- * Response Lenses
    associateSourceServersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateSourceServers' smart constructor.
data AssociateSourceServers = AssociateSourceServers'
  { -- | Application ID.
    applicationID :: Prelude.Text,
    -- | Source server IDs list.
    sourceServerIDs :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateSourceServers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationID', 'associateSourceServers_applicationID' - Application ID.
--
-- 'sourceServerIDs', 'associateSourceServers_sourceServerIDs' - Source server IDs list.
newAssociateSourceServers ::
  -- | 'applicationID'
  Prelude.Text ->
  -- | 'sourceServerIDs'
  Prelude.NonEmpty Prelude.Text ->
  AssociateSourceServers
newAssociateSourceServers
  pApplicationID_
  pSourceServerIDs_ =
    AssociateSourceServers'
      { applicationID =
          pApplicationID_,
        sourceServerIDs =
          Lens.coerced Lens.# pSourceServerIDs_
      }

-- | Application ID.
associateSourceServers_applicationID :: Lens.Lens' AssociateSourceServers Prelude.Text
associateSourceServers_applicationID = Lens.lens (\AssociateSourceServers' {applicationID} -> applicationID) (\s@AssociateSourceServers' {} a -> s {applicationID = a} :: AssociateSourceServers)

-- | Source server IDs list.
associateSourceServers_sourceServerIDs :: Lens.Lens' AssociateSourceServers (Prelude.NonEmpty Prelude.Text)
associateSourceServers_sourceServerIDs = Lens.lens (\AssociateSourceServers' {sourceServerIDs} -> sourceServerIDs) (\s@AssociateSourceServers' {} a -> s {sourceServerIDs = a} :: AssociateSourceServers) Prelude.. Lens.coerced

instance Core.AWSRequest AssociateSourceServers where
  type
    AWSResponse AssociateSourceServers =
      AssociateSourceServersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateSourceServersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateSourceServers where
  hashWithSalt _salt AssociateSourceServers' {..} =
    _salt
      `Prelude.hashWithSalt` applicationID
      `Prelude.hashWithSalt` sourceServerIDs

instance Prelude.NFData AssociateSourceServers where
  rnf AssociateSourceServers' {..} =
    Prelude.rnf applicationID
      `Prelude.seq` Prelude.rnf sourceServerIDs

instance Data.ToHeaders AssociateSourceServers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateSourceServers where
  toJSON AssociateSourceServers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("applicationID" Data..= applicationID),
            Prelude.Just
              ("sourceServerIDs" Data..= sourceServerIDs)
          ]
      )

instance Data.ToPath AssociateSourceServers where
  toPath = Prelude.const "/AssociateSourceServers"

instance Data.ToQuery AssociateSourceServers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateSourceServersResponse' smart constructor.
data AssociateSourceServersResponse = AssociateSourceServersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateSourceServersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateSourceServersResponse_httpStatus' - The response's http status code.
newAssociateSourceServersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateSourceServersResponse
newAssociateSourceServersResponse pHttpStatus_ =
  AssociateSourceServersResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateSourceServersResponse_httpStatus :: Lens.Lens' AssociateSourceServersResponse Prelude.Int
associateSourceServersResponse_httpStatus = Lens.lens (\AssociateSourceServersResponse' {httpStatus} -> httpStatus) (\s@AssociateSourceServersResponse' {} a -> s {httpStatus = a} :: AssociateSourceServersResponse)

instance
  Prelude.NFData
    AssociateSourceServersResponse
  where
  rnf AssociateSourceServersResponse' {..} =
    Prelude.rnf httpStatus
