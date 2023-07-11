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
-- Module      : Amazonka.AppStream.AssociateApplicationFleet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified application with the specified fleet. This is
-- only supported for Elastic fleets.
module Amazonka.AppStream.AssociateApplicationFleet
  ( -- * Creating a Request
    AssociateApplicationFleet (..),
    newAssociateApplicationFleet,

    -- * Request Lenses
    associateApplicationFleet_fleetName,
    associateApplicationFleet_applicationArn,

    -- * Destructuring the Response
    AssociateApplicationFleetResponse (..),
    newAssociateApplicationFleetResponse,

    -- * Response Lenses
    associateApplicationFleetResponse_applicationFleetAssociation,
    associateApplicationFleetResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateApplicationFleet' smart constructor.
data AssociateApplicationFleet = AssociateApplicationFleet'
  { -- | The name of the fleet.
    fleetName :: Prelude.Text,
    -- | The ARN of the application.
    applicationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateApplicationFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetName', 'associateApplicationFleet_fleetName' - The name of the fleet.
--
-- 'applicationArn', 'associateApplicationFleet_applicationArn' - The ARN of the application.
newAssociateApplicationFleet ::
  -- | 'fleetName'
  Prelude.Text ->
  -- | 'applicationArn'
  Prelude.Text ->
  AssociateApplicationFleet
newAssociateApplicationFleet
  pFleetName_
  pApplicationArn_ =
    AssociateApplicationFleet'
      { fleetName = pFleetName_,
        applicationArn = pApplicationArn_
      }

-- | The name of the fleet.
associateApplicationFleet_fleetName :: Lens.Lens' AssociateApplicationFleet Prelude.Text
associateApplicationFleet_fleetName = Lens.lens (\AssociateApplicationFleet' {fleetName} -> fleetName) (\s@AssociateApplicationFleet' {} a -> s {fleetName = a} :: AssociateApplicationFleet)

-- | The ARN of the application.
associateApplicationFleet_applicationArn :: Lens.Lens' AssociateApplicationFleet Prelude.Text
associateApplicationFleet_applicationArn = Lens.lens (\AssociateApplicationFleet' {applicationArn} -> applicationArn) (\s@AssociateApplicationFleet' {} a -> s {applicationArn = a} :: AssociateApplicationFleet)

instance Core.AWSRequest AssociateApplicationFleet where
  type
    AWSResponse AssociateApplicationFleet =
      AssociateApplicationFleetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateApplicationFleetResponse'
            Prelude.<$> (x Data..?> "ApplicationFleetAssociation")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateApplicationFleet where
  hashWithSalt _salt AssociateApplicationFleet' {..} =
    _salt
      `Prelude.hashWithSalt` fleetName
      `Prelude.hashWithSalt` applicationArn

instance Prelude.NFData AssociateApplicationFleet where
  rnf AssociateApplicationFleet' {..} =
    Prelude.rnf fleetName
      `Prelude.seq` Prelude.rnf applicationArn

instance Data.ToHeaders AssociateApplicationFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.AssociateApplicationFleet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateApplicationFleet where
  toJSON AssociateApplicationFleet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetName" Data..= fleetName),
            Prelude.Just
              ("ApplicationArn" Data..= applicationArn)
          ]
      )

instance Data.ToPath AssociateApplicationFleet where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateApplicationFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateApplicationFleetResponse' smart constructor.
data AssociateApplicationFleetResponse = AssociateApplicationFleetResponse'
  { -- | If fleet name is specified, this returns the list of applications that
    -- are associated to it. If application ARN is specified, this returns the
    -- list of fleets to which it is associated.
    applicationFleetAssociation :: Prelude.Maybe ApplicationFleetAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateApplicationFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationFleetAssociation', 'associateApplicationFleetResponse_applicationFleetAssociation' - If fleet name is specified, this returns the list of applications that
-- are associated to it. If application ARN is specified, this returns the
-- list of fleets to which it is associated.
--
-- 'httpStatus', 'associateApplicationFleetResponse_httpStatus' - The response's http status code.
newAssociateApplicationFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateApplicationFleetResponse
newAssociateApplicationFleetResponse pHttpStatus_ =
  AssociateApplicationFleetResponse'
    { applicationFleetAssociation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If fleet name is specified, this returns the list of applications that
-- are associated to it. If application ARN is specified, this returns the
-- list of fleets to which it is associated.
associateApplicationFleetResponse_applicationFleetAssociation :: Lens.Lens' AssociateApplicationFleetResponse (Prelude.Maybe ApplicationFleetAssociation)
associateApplicationFleetResponse_applicationFleetAssociation = Lens.lens (\AssociateApplicationFleetResponse' {applicationFleetAssociation} -> applicationFleetAssociation) (\s@AssociateApplicationFleetResponse' {} a -> s {applicationFleetAssociation = a} :: AssociateApplicationFleetResponse)

-- | The response's http status code.
associateApplicationFleetResponse_httpStatus :: Lens.Lens' AssociateApplicationFleetResponse Prelude.Int
associateApplicationFleetResponse_httpStatus = Lens.lens (\AssociateApplicationFleetResponse' {httpStatus} -> httpStatus) (\s@AssociateApplicationFleetResponse' {} a -> s {httpStatus = a} :: AssociateApplicationFleetResponse)

instance
  Prelude.NFData
    AssociateApplicationFleetResponse
  where
  rnf AssociateApplicationFleetResponse' {..} =
    Prelude.rnf applicationFleetAssociation
      `Prelude.seq` Prelude.rnf httpStatus
