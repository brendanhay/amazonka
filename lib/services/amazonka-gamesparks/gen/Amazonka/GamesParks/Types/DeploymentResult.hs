{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GamesParks.Types.DeploymentResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GamesParks.Types.DeploymentResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GamesParks.Types.ResultCode
import qualified Amazonka.Prelude as Prelude

-- | The result of the deployment.
--
-- /See:/ 'newDeploymentResult' smart constructor.
data DeploymentResult = DeploymentResult'
  { -- | Details about the deployment result.
    message :: Prelude.Maybe Prelude.Text,
    -- | The type of deployment result.
    resultCode :: Prelude.Maybe ResultCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'deploymentResult_message' - Details about the deployment result.
--
-- 'resultCode', 'deploymentResult_resultCode' - The type of deployment result.
newDeploymentResult ::
  DeploymentResult
newDeploymentResult =
  DeploymentResult'
    { message = Prelude.Nothing,
      resultCode = Prelude.Nothing
    }

-- | Details about the deployment result.
deploymentResult_message :: Lens.Lens' DeploymentResult (Prelude.Maybe Prelude.Text)
deploymentResult_message = Lens.lens (\DeploymentResult' {message} -> message) (\s@DeploymentResult' {} a -> s {message = a} :: DeploymentResult)

-- | The type of deployment result.
deploymentResult_resultCode :: Lens.Lens' DeploymentResult (Prelude.Maybe ResultCode)
deploymentResult_resultCode = Lens.lens (\DeploymentResult' {resultCode} -> resultCode) (\s@DeploymentResult' {} a -> s {resultCode = a} :: DeploymentResult)

instance Core.FromJSON DeploymentResult where
  parseJSON =
    Core.withObject
      "DeploymentResult"
      ( \x ->
          DeploymentResult'
            Prelude.<$> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "ResultCode")
      )

instance Prelude.Hashable DeploymentResult where
  hashWithSalt _salt DeploymentResult' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` resultCode

instance Prelude.NFData DeploymentResult where
  rnf DeploymentResult' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf resultCode
