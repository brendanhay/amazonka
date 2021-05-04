{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AppStream.AssociateFleet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified fleet with the specified stack.
module Network.AWS.AppStream.AssociateFleet
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

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateFleet' smart constructor.
data AssociateFleet = AssociateFleet'
  { -- | The name of the fleet.
    fleetName :: Prelude.Text,
    -- | The name of the stack.
    stackName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest AssociateFleet where
  type Rs AssociateFleet = AssociateFleetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateFleetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateFleet

instance Prelude.NFData AssociateFleet

instance Prelude.ToHeaders AssociateFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "PhotonAdminProxyService.AssociateFleet" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AssociateFleet where
  toJSON AssociateFleet' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FleetName" Prelude..= fleetName),
            Prelude.Just ("StackName" Prelude..= stackName)
          ]
      )

instance Prelude.ToPath AssociateFleet where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AssociateFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateFleetResponse' smart constructor.
data AssociateFleetResponse = AssociateFleetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData AssociateFleetResponse
