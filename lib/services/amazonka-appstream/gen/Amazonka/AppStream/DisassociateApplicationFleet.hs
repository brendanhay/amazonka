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
-- Module      : Amazonka.AppStream.DisassociateApplicationFleet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified application from the fleet.
module Amazonka.AppStream.DisassociateApplicationFleet
  ( -- * Creating a Request
    DisassociateApplicationFleet (..),
    newDisassociateApplicationFleet,

    -- * Request Lenses
    disassociateApplicationFleet_fleetName,
    disassociateApplicationFleet_applicationArn,

    -- * Destructuring the Response
    DisassociateApplicationFleetResponse (..),
    newDisassociateApplicationFleetResponse,

    -- * Response Lenses
    disassociateApplicationFleetResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateApplicationFleet' smart constructor.
data DisassociateApplicationFleet = DisassociateApplicationFleet'
  { -- | The name of the fleet.
    fleetName :: Prelude.Text,
    -- | The ARN of the application.
    applicationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateApplicationFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetName', 'disassociateApplicationFleet_fleetName' - The name of the fleet.
--
-- 'applicationArn', 'disassociateApplicationFleet_applicationArn' - The ARN of the application.
newDisassociateApplicationFleet ::
  -- | 'fleetName'
  Prelude.Text ->
  -- | 'applicationArn'
  Prelude.Text ->
  DisassociateApplicationFleet
newDisassociateApplicationFleet
  pFleetName_
  pApplicationArn_ =
    DisassociateApplicationFleet'
      { fleetName =
          pFleetName_,
        applicationArn = pApplicationArn_
      }

-- | The name of the fleet.
disassociateApplicationFleet_fleetName :: Lens.Lens' DisassociateApplicationFleet Prelude.Text
disassociateApplicationFleet_fleetName = Lens.lens (\DisassociateApplicationFleet' {fleetName} -> fleetName) (\s@DisassociateApplicationFleet' {} a -> s {fleetName = a} :: DisassociateApplicationFleet)

-- | The ARN of the application.
disassociateApplicationFleet_applicationArn :: Lens.Lens' DisassociateApplicationFleet Prelude.Text
disassociateApplicationFleet_applicationArn = Lens.lens (\DisassociateApplicationFleet' {applicationArn} -> applicationArn) (\s@DisassociateApplicationFleet' {} a -> s {applicationArn = a} :: DisassociateApplicationFleet)

instance Core.AWSRequest DisassociateApplicationFleet where
  type
    AWSResponse DisassociateApplicationFleet =
      DisassociateApplicationFleetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateApplicationFleetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateApplicationFleet
  where
  hashWithSalt _salt DisassociateApplicationFleet' {..} =
    _salt
      `Prelude.hashWithSalt` fleetName
      `Prelude.hashWithSalt` applicationArn

instance Prelude.NFData DisassociateApplicationFleet where
  rnf DisassociateApplicationFleet' {..} =
    Prelude.rnf fleetName
      `Prelude.seq` Prelude.rnf applicationArn

instance Data.ToHeaders DisassociateApplicationFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.DisassociateApplicationFleet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateApplicationFleet where
  toJSON DisassociateApplicationFleet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetName" Data..= fleetName),
            Prelude.Just
              ("ApplicationArn" Data..= applicationArn)
          ]
      )

instance Data.ToPath DisassociateApplicationFleet where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateApplicationFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateApplicationFleetResponse' smart constructor.
data DisassociateApplicationFleetResponse = DisassociateApplicationFleetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateApplicationFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateApplicationFleetResponse_httpStatus' - The response's http status code.
newDisassociateApplicationFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateApplicationFleetResponse
newDisassociateApplicationFleetResponse pHttpStatus_ =
  DisassociateApplicationFleetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateApplicationFleetResponse_httpStatus :: Lens.Lens' DisassociateApplicationFleetResponse Prelude.Int
disassociateApplicationFleetResponse_httpStatus = Lens.lens (\DisassociateApplicationFleetResponse' {httpStatus} -> httpStatus) (\s@DisassociateApplicationFleetResponse' {} a -> s {httpStatus = a} :: DisassociateApplicationFleetResponse)

instance
  Prelude.NFData
    DisassociateApplicationFleetResponse
  where
  rnf DisassociateApplicationFleetResponse' {..} =
    Prelude.rnf httpStatus
