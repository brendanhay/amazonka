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
-- Module      : Amazonka.AppStream.DisassociateFleet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified fleet from the specified stack.
module Amazonka.AppStream.DisassociateFleet
  ( -- * Creating a Request
    DisassociateFleet (..),
    newDisassociateFleet,

    -- * Request Lenses
    disassociateFleet_fleetName,
    disassociateFleet_stackName,

    -- * Destructuring the Response
    DisassociateFleetResponse (..),
    newDisassociateFleetResponse,

    -- * Response Lenses
    disassociateFleetResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateFleet' smart constructor.
data DisassociateFleet = DisassociateFleet'
  { -- | The name of the fleet.
    fleetName :: Prelude.Text,
    -- | The name of the stack.
    stackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetName', 'disassociateFleet_fleetName' - The name of the fleet.
--
-- 'stackName', 'disassociateFleet_stackName' - The name of the stack.
newDisassociateFleet ::
  -- | 'fleetName'
  Prelude.Text ->
  -- | 'stackName'
  Prelude.Text ->
  DisassociateFleet
newDisassociateFleet pFleetName_ pStackName_ =
  DisassociateFleet'
    { fleetName = pFleetName_,
      stackName = pStackName_
    }

-- | The name of the fleet.
disassociateFleet_fleetName :: Lens.Lens' DisassociateFleet Prelude.Text
disassociateFleet_fleetName = Lens.lens (\DisassociateFleet' {fleetName} -> fleetName) (\s@DisassociateFleet' {} a -> s {fleetName = a} :: DisassociateFleet)

-- | The name of the stack.
disassociateFleet_stackName :: Lens.Lens' DisassociateFleet Prelude.Text
disassociateFleet_stackName = Lens.lens (\DisassociateFleet' {stackName} -> stackName) (\s@DisassociateFleet' {} a -> s {stackName = a} :: DisassociateFleet)

instance Core.AWSRequest DisassociateFleet where
  type
    AWSResponse DisassociateFleet =
      DisassociateFleetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateFleetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateFleet where
  hashWithSalt _salt DisassociateFleet' {..} =
    _salt
      `Prelude.hashWithSalt` fleetName
      `Prelude.hashWithSalt` stackName

instance Prelude.NFData DisassociateFleet where
  rnf DisassociateFleet' {..} =
    Prelude.rnf fleetName
      `Prelude.seq` Prelude.rnf stackName

instance Data.ToHeaders DisassociateFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.DisassociateFleet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateFleet where
  toJSON DisassociateFleet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetName" Data..= fleetName),
            Prelude.Just ("StackName" Data..= stackName)
          ]
      )

instance Data.ToPath DisassociateFleet where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateFleetResponse' smart constructor.
data DisassociateFleetResponse = DisassociateFleetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateFleetResponse_httpStatus' - The response's http status code.
newDisassociateFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateFleetResponse
newDisassociateFleetResponse pHttpStatus_ =
  DisassociateFleetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateFleetResponse_httpStatus :: Lens.Lens' DisassociateFleetResponse Prelude.Int
disassociateFleetResponse_httpStatus = Lens.lens (\DisassociateFleetResponse' {httpStatus} -> httpStatus) (\s@DisassociateFleetResponse' {} a -> s {httpStatus = a} :: DisassociateFleetResponse)

instance Prelude.NFData DisassociateFleetResponse where
  rnf DisassociateFleetResponse' {..} =
    Prelude.rnf httpStatus
