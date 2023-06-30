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
-- Module      : Amazonka.AppConfig.Types.Environment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.Environment where

import Amazonka.AppConfig.Types.EnvironmentState
import Amazonka.AppConfig.Types.Monitor
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newEnvironment' smart constructor.
data Environment = Environment'
  { -- | The application ID.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The description of the environment.
    description :: Prelude.Maybe Prelude.Text,
    -- | The environment ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | Amazon CloudWatch alarms monitored during the deployment.
    monitors :: Prelude.Maybe [Monitor],
    -- | The name of the environment.
    name :: Prelude.Maybe Prelude.Text,
    -- | The state of the environment. An environment can be in one of the
    -- following states: @READY_FOR_DEPLOYMENT@, @DEPLOYING@, @ROLLING_BACK@,
    -- or @ROLLED_BACK@
    state :: Prelude.Maybe EnvironmentState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Environment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'environment_applicationId' - The application ID.
--
-- 'description', 'environment_description' - The description of the environment.
--
-- 'id', 'environment_id' - The environment ID.
--
-- 'monitors', 'environment_monitors' - Amazon CloudWatch alarms monitored during the deployment.
--
-- 'name', 'environment_name' - The name of the environment.
--
-- 'state', 'environment_state' - The state of the environment. An environment can be in one of the
-- following states: @READY_FOR_DEPLOYMENT@, @DEPLOYING@, @ROLLING_BACK@,
-- or @ROLLED_BACK@
newEnvironment ::
  Environment
newEnvironment =
  Environment'
    { applicationId = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      monitors = Prelude.Nothing,
      name = Prelude.Nothing,
      state = Prelude.Nothing
    }

-- | The application ID.
environment_applicationId :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_applicationId = Lens.lens (\Environment' {applicationId} -> applicationId) (\s@Environment' {} a -> s {applicationId = a} :: Environment)

-- | The description of the environment.
environment_description :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_description = Lens.lens (\Environment' {description} -> description) (\s@Environment' {} a -> s {description = a} :: Environment)

-- | The environment ID.
environment_id :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_id = Lens.lens (\Environment' {id} -> id) (\s@Environment' {} a -> s {id = a} :: Environment)

-- | Amazon CloudWatch alarms monitored during the deployment.
environment_monitors :: Lens.Lens' Environment (Prelude.Maybe [Monitor])
environment_monitors = Lens.lens (\Environment' {monitors} -> monitors) (\s@Environment' {} a -> s {monitors = a} :: Environment) Prelude.. Lens.mapping Lens.coerced

-- | The name of the environment.
environment_name :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_name = Lens.lens (\Environment' {name} -> name) (\s@Environment' {} a -> s {name = a} :: Environment)

-- | The state of the environment. An environment can be in one of the
-- following states: @READY_FOR_DEPLOYMENT@, @DEPLOYING@, @ROLLING_BACK@,
-- or @ROLLED_BACK@
environment_state :: Lens.Lens' Environment (Prelude.Maybe EnvironmentState)
environment_state = Lens.lens (\Environment' {state} -> state) (\s@Environment' {} a -> s {state = a} :: Environment)

instance Data.FromJSON Environment where
  parseJSON =
    Data.withObject
      "Environment"
      ( \x ->
          Environment'
            Prelude.<$> (x Data..:? "ApplicationId")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Monitors" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "State")
      )

instance Prelude.Hashable Environment where
  hashWithSalt _salt Environment' {..} =
    _salt
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` monitors
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` state

instance Prelude.NFData Environment where
  rnf Environment' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf monitors
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf state
