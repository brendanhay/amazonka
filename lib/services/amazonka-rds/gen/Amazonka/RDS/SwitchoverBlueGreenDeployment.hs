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
-- Module      : Amazonka.RDS.SwitchoverBlueGreenDeployment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Switches over a blue\/green deployment.
--
-- Before you switch over, production traffic is routed to the databases in
-- the blue environment. After you switch over, production traffic is
-- routed to the databases in the green environment.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/blue-green-deployments.html Using Amazon RDS Blue\/Green Deployments for database updates>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/blue-green-deployments.html Using Amazon RDS Blue\/Green Deployments for database updates>
-- in the /Amazon Aurora User Guide/.
module Amazonka.RDS.SwitchoverBlueGreenDeployment
  ( -- * Creating a Request
    SwitchoverBlueGreenDeployment (..),
    newSwitchoverBlueGreenDeployment,

    -- * Request Lenses
    switchoverBlueGreenDeployment_switchoverTimeout,
    switchoverBlueGreenDeployment_blueGreenDeploymentIdentifier,

    -- * Destructuring the Response
    SwitchoverBlueGreenDeploymentResponse (..),
    newSwitchoverBlueGreenDeploymentResponse,

    -- * Response Lenses
    switchoverBlueGreenDeploymentResponse_blueGreenDeployment,
    switchoverBlueGreenDeploymentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSwitchoverBlueGreenDeployment' smart constructor.
data SwitchoverBlueGreenDeployment = SwitchoverBlueGreenDeployment'
  { -- | The amount of time, in seconds, for the switchover to complete. The
    -- default is 300.
    --
    -- If the switchover takes longer than the specified duration, then any
    -- changes are rolled back, and no changes are made to the environments.
    switchoverTimeout :: Prelude.Maybe Prelude.Natural,
    -- | The blue\/green deployment identifier.
    --
    -- Constraints:
    --
    -- -   Must match an existing blue\/green deployment identifier.
    blueGreenDeploymentIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SwitchoverBlueGreenDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'switchoverTimeout', 'switchoverBlueGreenDeployment_switchoverTimeout' - The amount of time, in seconds, for the switchover to complete. The
-- default is 300.
--
-- If the switchover takes longer than the specified duration, then any
-- changes are rolled back, and no changes are made to the environments.
--
-- 'blueGreenDeploymentIdentifier', 'switchoverBlueGreenDeployment_blueGreenDeploymentIdentifier' - The blue\/green deployment identifier.
--
-- Constraints:
--
-- -   Must match an existing blue\/green deployment identifier.
newSwitchoverBlueGreenDeployment ::
  -- | 'blueGreenDeploymentIdentifier'
  Prelude.Text ->
  SwitchoverBlueGreenDeployment
newSwitchoverBlueGreenDeployment
  pBlueGreenDeploymentIdentifier_ =
    SwitchoverBlueGreenDeployment'
      { switchoverTimeout =
          Prelude.Nothing,
        blueGreenDeploymentIdentifier =
          pBlueGreenDeploymentIdentifier_
      }

-- | The amount of time, in seconds, for the switchover to complete. The
-- default is 300.
--
-- If the switchover takes longer than the specified duration, then any
-- changes are rolled back, and no changes are made to the environments.
switchoverBlueGreenDeployment_switchoverTimeout :: Lens.Lens' SwitchoverBlueGreenDeployment (Prelude.Maybe Prelude.Natural)
switchoverBlueGreenDeployment_switchoverTimeout = Lens.lens (\SwitchoverBlueGreenDeployment' {switchoverTimeout} -> switchoverTimeout) (\s@SwitchoverBlueGreenDeployment' {} a -> s {switchoverTimeout = a} :: SwitchoverBlueGreenDeployment)

-- | The blue\/green deployment identifier.
--
-- Constraints:
--
-- -   Must match an existing blue\/green deployment identifier.
switchoverBlueGreenDeployment_blueGreenDeploymentIdentifier :: Lens.Lens' SwitchoverBlueGreenDeployment Prelude.Text
switchoverBlueGreenDeployment_blueGreenDeploymentIdentifier = Lens.lens (\SwitchoverBlueGreenDeployment' {blueGreenDeploymentIdentifier} -> blueGreenDeploymentIdentifier) (\s@SwitchoverBlueGreenDeployment' {} a -> s {blueGreenDeploymentIdentifier = a} :: SwitchoverBlueGreenDeployment)

instance
  Core.AWSRequest
    SwitchoverBlueGreenDeployment
  where
  type
    AWSResponse SwitchoverBlueGreenDeployment =
      SwitchoverBlueGreenDeploymentResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "SwitchoverBlueGreenDeploymentResult"
      ( \s h x ->
          SwitchoverBlueGreenDeploymentResponse'
            Prelude.<$> (x Data..@? "BlueGreenDeployment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    SwitchoverBlueGreenDeployment
  where
  hashWithSalt _salt SwitchoverBlueGreenDeployment' {..} =
    _salt
      `Prelude.hashWithSalt` switchoverTimeout
      `Prelude.hashWithSalt` blueGreenDeploymentIdentifier

instance Prelude.NFData SwitchoverBlueGreenDeployment where
  rnf SwitchoverBlueGreenDeployment' {..} =
    Prelude.rnf switchoverTimeout
      `Prelude.seq` Prelude.rnf blueGreenDeploymentIdentifier

instance Data.ToHeaders SwitchoverBlueGreenDeployment where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath SwitchoverBlueGreenDeployment where
  toPath = Prelude.const "/"

instance Data.ToQuery SwitchoverBlueGreenDeployment where
  toQuery SwitchoverBlueGreenDeployment' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "SwitchoverBlueGreenDeployment" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "SwitchoverTimeout" Data.=: switchoverTimeout,
        "BlueGreenDeploymentIdentifier"
          Data.=: blueGreenDeploymentIdentifier
      ]

-- | /See:/ 'newSwitchoverBlueGreenDeploymentResponse' smart constructor.
data SwitchoverBlueGreenDeploymentResponse = SwitchoverBlueGreenDeploymentResponse'
  { blueGreenDeployment :: Prelude.Maybe BlueGreenDeployment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SwitchoverBlueGreenDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blueGreenDeployment', 'switchoverBlueGreenDeploymentResponse_blueGreenDeployment' - Undocumented member.
--
-- 'httpStatus', 'switchoverBlueGreenDeploymentResponse_httpStatus' - The response's http status code.
newSwitchoverBlueGreenDeploymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SwitchoverBlueGreenDeploymentResponse
newSwitchoverBlueGreenDeploymentResponse pHttpStatus_ =
  SwitchoverBlueGreenDeploymentResponse'
    { blueGreenDeployment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
switchoverBlueGreenDeploymentResponse_blueGreenDeployment :: Lens.Lens' SwitchoverBlueGreenDeploymentResponse (Prelude.Maybe BlueGreenDeployment)
switchoverBlueGreenDeploymentResponse_blueGreenDeployment = Lens.lens (\SwitchoverBlueGreenDeploymentResponse' {blueGreenDeployment} -> blueGreenDeployment) (\s@SwitchoverBlueGreenDeploymentResponse' {} a -> s {blueGreenDeployment = a} :: SwitchoverBlueGreenDeploymentResponse)

-- | The response's http status code.
switchoverBlueGreenDeploymentResponse_httpStatus :: Lens.Lens' SwitchoverBlueGreenDeploymentResponse Prelude.Int
switchoverBlueGreenDeploymentResponse_httpStatus = Lens.lens (\SwitchoverBlueGreenDeploymentResponse' {httpStatus} -> httpStatus) (\s@SwitchoverBlueGreenDeploymentResponse' {} a -> s {httpStatus = a} :: SwitchoverBlueGreenDeploymentResponse)

instance
  Prelude.NFData
    SwitchoverBlueGreenDeploymentResponse
  where
  rnf SwitchoverBlueGreenDeploymentResponse' {..} =
    Prelude.rnf blueGreenDeployment
      `Prelude.seq` Prelude.rnf httpStatus
