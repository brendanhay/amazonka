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
-- Module      : Amazonka.Glue.Types.Session
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Session where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.ConnectionsList
import Amazonka.Glue.Types.SessionCommand
import Amazonka.Glue.Types.SessionStatus
import qualified Amazonka.Prelude as Prelude

-- | The period in which a remote Spark runtime environment is running.
--
-- /See:/ 'newSession' smart constructor.
data Session = Session'
  { -- | The command object.See SessionCommand.
    command :: Prelude.Maybe SessionCommand,
    -- | The number of connections used for the session.
    connections :: Prelude.Maybe ConnectionsList,
    -- | The time and date when the session was created.
    createdOn :: Prelude.Maybe Data.POSIX,
    -- | A map array of key-value pairs. Max is 75 pairs.
    defaultArguments :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The description of the session.
    description :: Prelude.Maybe Prelude.Text,
    -- | The error message displayed during the session.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The Glue version determines the versions of Apache Spark and Python that
    -- Glue supports. The GlueVersion must be greater than 2.0.
    glueVersion :: Prelude.Maybe Prelude.Text,
    -- | The ID of the session.
    id :: Prelude.Maybe Prelude.Text,
    -- | The number of Glue data processing units (DPUs) that can be allocated
    -- when the job runs. A DPU is a relative measure of processing power that
    -- consists of 4 vCPUs of compute capacity and 16 GB memory.
    maxCapacity :: Prelude.Maybe Prelude.Double,
    -- | The code execution progress of the session.
    progress :: Prelude.Maybe Prelude.Double,
    -- | The name or Amazon Resource Name (ARN) of the IAM role associated with
    -- the Session.
    role' :: Prelude.Maybe Prelude.Text,
    -- | The name of the SecurityConfiguration structure to be used with the
    -- session.
    securityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The session status.
    status :: Prelude.Maybe SessionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Session' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'command', 'session_command' - The command object.See SessionCommand.
--
-- 'connections', 'session_connections' - The number of connections used for the session.
--
-- 'createdOn', 'session_createdOn' - The time and date when the session was created.
--
-- 'defaultArguments', 'session_defaultArguments' - A map array of key-value pairs. Max is 75 pairs.
--
-- 'description', 'session_description' - The description of the session.
--
-- 'errorMessage', 'session_errorMessage' - The error message displayed during the session.
--
-- 'glueVersion', 'session_glueVersion' - The Glue version determines the versions of Apache Spark and Python that
-- Glue supports. The GlueVersion must be greater than 2.0.
--
-- 'id', 'session_id' - The ID of the session.
--
-- 'maxCapacity', 'session_maxCapacity' - The number of Glue data processing units (DPUs) that can be allocated
-- when the job runs. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB memory.
--
-- 'progress', 'session_progress' - The code execution progress of the session.
--
-- 'role'', 'session_role' - The name or Amazon Resource Name (ARN) of the IAM role associated with
-- the Session.
--
-- 'securityConfiguration', 'session_securityConfiguration' - The name of the SecurityConfiguration structure to be used with the
-- session.
--
-- 'status', 'session_status' - The session status.
newSession ::
  Session
newSession =
  Session'
    { command = Prelude.Nothing,
      connections = Prelude.Nothing,
      createdOn = Prelude.Nothing,
      defaultArguments = Prelude.Nothing,
      description = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      glueVersion = Prelude.Nothing,
      id = Prelude.Nothing,
      maxCapacity = Prelude.Nothing,
      progress = Prelude.Nothing,
      role' = Prelude.Nothing,
      securityConfiguration = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The command object.See SessionCommand.
session_command :: Lens.Lens' Session (Prelude.Maybe SessionCommand)
session_command = Lens.lens (\Session' {command} -> command) (\s@Session' {} a -> s {command = a} :: Session)

-- | The number of connections used for the session.
session_connections :: Lens.Lens' Session (Prelude.Maybe ConnectionsList)
session_connections = Lens.lens (\Session' {connections} -> connections) (\s@Session' {} a -> s {connections = a} :: Session)

-- | The time and date when the session was created.
session_createdOn :: Lens.Lens' Session (Prelude.Maybe Prelude.UTCTime)
session_createdOn = Lens.lens (\Session' {createdOn} -> createdOn) (\s@Session' {} a -> s {createdOn = a} :: Session) Prelude.. Lens.mapping Data._Time

-- | A map array of key-value pairs. Max is 75 pairs.
session_defaultArguments :: Lens.Lens' Session (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
session_defaultArguments = Lens.lens (\Session' {defaultArguments} -> defaultArguments) (\s@Session' {} a -> s {defaultArguments = a} :: Session) Prelude.. Lens.mapping Lens.coerced

-- | The description of the session.
session_description :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_description = Lens.lens (\Session' {description} -> description) (\s@Session' {} a -> s {description = a} :: Session)

-- | The error message displayed during the session.
session_errorMessage :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_errorMessage = Lens.lens (\Session' {errorMessage} -> errorMessage) (\s@Session' {} a -> s {errorMessage = a} :: Session)

-- | The Glue version determines the versions of Apache Spark and Python that
-- Glue supports. The GlueVersion must be greater than 2.0.
session_glueVersion :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_glueVersion = Lens.lens (\Session' {glueVersion} -> glueVersion) (\s@Session' {} a -> s {glueVersion = a} :: Session)

-- | The ID of the session.
session_id :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_id = Lens.lens (\Session' {id} -> id) (\s@Session' {} a -> s {id = a} :: Session)

-- | The number of Glue data processing units (DPUs) that can be allocated
-- when the job runs. A DPU is a relative measure of processing power that
-- consists of 4 vCPUs of compute capacity and 16 GB memory.
session_maxCapacity :: Lens.Lens' Session (Prelude.Maybe Prelude.Double)
session_maxCapacity = Lens.lens (\Session' {maxCapacity} -> maxCapacity) (\s@Session' {} a -> s {maxCapacity = a} :: Session)

-- | The code execution progress of the session.
session_progress :: Lens.Lens' Session (Prelude.Maybe Prelude.Double)
session_progress = Lens.lens (\Session' {progress} -> progress) (\s@Session' {} a -> s {progress = a} :: Session)

-- | The name or Amazon Resource Name (ARN) of the IAM role associated with
-- the Session.
session_role :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_role = Lens.lens (\Session' {role'} -> role') (\s@Session' {} a -> s {role' = a} :: Session)

-- | The name of the SecurityConfiguration structure to be used with the
-- session.
session_securityConfiguration :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_securityConfiguration = Lens.lens (\Session' {securityConfiguration} -> securityConfiguration) (\s@Session' {} a -> s {securityConfiguration = a} :: Session)

-- | The session status.
session_status :: Lens.Lens' Session (Prelude.Maybe SessionStatus)
session_status = Lens.lens (\Session' {status} -> status) (\s@Session' {} a -> s {status = a} :: Session)

instance Data.FromJSON Session where
  parseJSON =
    Data.withObject
      "Session"
      ( \x ->
          Session'
            Prelude.<$> (x Data..:? "Command")
            Prelude.<*> (x Data..:? "Connections")
            Prelude.<*> (x Data..:? "CreatedOn")
            Prelude.<*> ( x Data..:? "DefaultArguments"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "GlueVersion")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "MaxCapacity")
            Prelude.<*> (x Data..:? "Progress")
            Prelude.<*> (x Data..:? "Role")
            Prelude.<*> (x Data..:? "SecurityConfiguration")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable Session where
  hashWithSalt _salt Session' {..} =
    _salt `Prelude.hashWithSalt` command
      `Prelude.hashWithSalt` connections
      `Prelude.hashWithSalt` createdOn
      `Prelude.hashWithSalt` defaultArguments
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` glueVersion
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` maxCapacity
      `Prelude.hashWithSalt` progress
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` securityConfiguration
      `Prelude.hashWithSalt` status

instance Prelude.NFData Session where
  rnf Session' {..} =
    Prelude.rnf command
      `Prelude.seq` Prelude.rnf connections
      `Prelude.seq` Prelude.rnf createdOn
      `Prelude.seq` Prelude.rnf defaultArguments
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf glueVersion
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf maxCapacity
      `Prelude.seq` Prelude.rnf progress
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf securityConfiguration
      `Prelude.seq` Prelude.rnf status
