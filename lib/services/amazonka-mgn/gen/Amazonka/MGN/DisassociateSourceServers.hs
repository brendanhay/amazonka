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
-- Module      : Amazonka.MGN.DisassociateSourceServers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociate source servers from application.
module Amazonka.MGN.DisassociateSourceServers
  ( -- * Creating a Request
    DisassociateSourceServers (..),
    newDisassociateSourceServers,

    -- * Request Lenses
    disassociateSourceServers_applicationID,
    disassociateSourceServers_sourceServerIDs,

    -- * Destructuring the Response
    DisassociateSourceServersResponse (..),
    newDisassociateSourceServersResponse,

    -- * Response Lenses
    disassociateSourceServersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateSourceServers' smart constructor.
data DisassociateSourceServers = DisassociateSourceServers'
  { -- | Application ID.
    applicationID :: Prelude.Text,
    -- | Source server IDs list.
    sourceServerIDs :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateSourceServers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationID', 'disassociateSourceServers_applicationID' - Application ID.
--
-- 'sourceServerIDs', 'disassociateSourceServers_sourceServerIDs' - Source server IDs list.
newDisassociateSourceServers ::
  -- | 'applicationID'
  Prelude.Text ->
  -- | 'sourceServerIDs'
  Prelude.NonEmpty Prelude.Text ->
  DisassociateSourceServers
newDisassociateSourceServers
  pApplicationID_
  pSourceServerIDs_ =
    DisassociateSourceServers'
      { applicationID =
          pApplicationID_,
        sourceServerIDs =
          Lens.coerced Lens.# pSourceServerIDs_
      }

-- | Application ID.
disassociateSourceServers_applicationID :: Lens.Lens' DisassociateSourceServers Prelude.Text
disassociateSourceServers_applicationID = Lens.lens (\DisassociateSourceServers' {applicationID} -> applicationID) (\s@DisassociateSourceServers' {} a -> s {applicationID = a} :: DisassociateSourceServers)

-- | Source server IDs list.
disassociateSourceServers_sourceServerIDs :: Lens.Lens' DisassociateSourceServers (Prelude.NonEmpty Prelude.Text)
disassociateSourceServers_sourceServerIDs = Lens.lens (\DisassociateSourceServers' {sourceServerIDs} -> sourceServerIDs) (\s@DisassociateSourceServers' {} a -> s {sourceServerIDs = a} :: DisassociateSourceServers) Prelude.. Lens.coerced

instance Core.AWSRequest DisassociateSourceServers where
  type
    AWSResponse DisassociateSourceServers =
      DisassociateSourceServersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateSourceServersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateSourceServers where
  hashWithSalt _salt DisassociateSourceServers' {..} =
    _salt `Prelude.hashWithSalt` applicationID
      `Prelude.hashWithSalt` sourceServerIDs

instance Prelude.NFData DisassociateSourceServers where
  rnf DisassociateSourceServers' {..} =
    Prelude.rnf applicationID
      `Prelude.seq` Prelude.rnf sourceServerIDs

instance Data.ToHeaders DisassociateSourceServers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateSourceServers where
  toJSON DisassociateSourceServers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("applicationID" Data..= applicationID),
            Prelude.Just
              ("sourceServerIDs" Data..= sourceServerIDs)
          ]
      )

instance Data.ToPath DisassociateSourceServers where
  toPath = Prelude.const "/DisassociateSourceServers"

instance Data.ToQuery DisassociateSourceServers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateSourceServersResponse' smart constructor.
data DisassociateSourceServersResponse = DisassociateSourceServersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateSourceServersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateSourceServersResponse_httpStatus' - The response's http status code.
newDisassociateSourceServersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateSourceServersResponse
newDisassociateSourceServersResponse pHttpStatus_ =
  DisassociateSourceServersResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateSourceServersResponse_httpStatus :: Lens.Lens' DisassociateSourceServersResponse Prelude.Int
disassociateSourceServersResponse_httpStatus = Lens.lens (\DisassociateSourceServersResponse' {httpStatus} -> httpStatus) (\s@DisassociateSourceServersResponse' {} a -> s {httpStatus = a} :: DisassociateSourceServersResponse)

instance
  Prelude.NFData
    DisassociateSourceServersResponse
  where
  rnf DisassociateSourceServersResponse' {..} =
    Prelude.rnf httpStatus
