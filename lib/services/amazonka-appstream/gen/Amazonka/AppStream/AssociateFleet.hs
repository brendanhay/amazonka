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
-- Module      : Amazonka.AppStream.AssociateFleet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified fleet with the specified stack.
module Amazonka.AppStream.AssociateFleet
  ( -- * Creating a Request
    AssociateFleet (..),
    newAssociateFleet,

    -- * Request Lenses
    associateFleet_fleetName,
    associateFleet_stackName,

    -- * Destructuring the Response
    AssociateFleetResponse (..),
    newAssociateFleetResponse,

    -- * Response Lenses
    associateFleetResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateFleet' smart constructor.
data AssociateFleet = AssociateFleet'
  { -- | The name of the fleet.
    fleetName :: Prelude.Text,
    -- | The name of the stack.
    stackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetName', 'associateFleet_fleetName' - The name of the fleet.
--
-- 'stackName', 'associateFleet_stackName' - The name of the stack.
newAssociateFleet ::
  -- | 'fleetName'
  Prelude.Text ->
  -- | 'stackName'
  Prelude.Text ->
  AssociateFleet
newAssociateFleet pFleetName_ pStackName_ =
  AssociateFleet'
    { fleetName = pFleetName_,
      stackName = pStackName_
    }

-- | The name of the fleet.
associateFleet_fleetName :: Lens.Lens' AssociateFleet Prelude.Text
associateFleet_fleetName = Lens.lens (\AssociateFleet' {fleetName} -> fleetName) (\s@AssociateFleet' {} a -> s {fleetName = a} :: AssociateFleet)

-- | The name of the stack.
associateFleet_stackName :: Lens.Lens' AssociateFleet Prelude.Text
associateFleet_stackName = Lens.lens (\AssociateFleet' {stackName} -> stackName) (\s@AssociateFleet' {} a -> s {stackName = a} :: AssociateFleet)

instance Core.AWSRequest AssociateFleet where
  type
    AWSResponse AssociateFleet =
      AssociateFleetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateFleetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateFleet where
  hashWithSalt _salt AssociateFleet' {..} =
    _salt `Prelude.hashWithSalt` fleetName
      `Prelude.hashWithSalt` stackName

instance Prelude.NFData AssociateFleet where
  rnf AssociateFleet' {..} =
    Prelude.rnf fleetName
      `Prelude.seq` Prelude.rnf stackName

instance Data.ToHeaders AssociateFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.AssociateFleet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateFleet where
  toJSON AssociateFleet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetName" Data..= fleetName),
            Prelude.Just ("StackName" Data..= stackName)
          ]
      )

instance Data.ToPath AssociateFleet where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateFleetResponse' smart constructor.
data AssociateFleetResponse = AssociateFleetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateFleetResponse_httpStatus' - The response's http status code.
newAssociateFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateFleetResponse
newAssociateFleetResponse pHttpStatus_ =
  AssociateFleetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
associateFleetResponse_httpStatus :: Lens.Lens' AssociateFleetResponse Prelude.Int
associateFleetResponse_httpStatus = Lens.lens (\AssociateFleetResponse' {httpStatus} -> httpStatus) (\s@AssociateFleetResponse' {} a -> s {httpStatus = a} :: AssociateFleetResponse)

instance Prelude.NFData AssociateFleetResponse where
  rnf AssociateFleetResponse' {..} =
    Prelude.rnf httpStatus
