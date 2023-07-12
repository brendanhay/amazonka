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
-- Module      : Amazonka.Athena.Types.SessionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.SessionConfiguration where

import Amazonka.Athena.Types.EncryptionConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains session configuration information.
--
-- /See:/ 'newSessionConfiguration' smart constructor.
data SessionConfiguration = SessionConfiguration'
  { encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration,
    -- | The ARN of the execution role used for the session.
    executionRole :: Prelude.Maybe Prelude.Text,
    -- | The idle timeout in seconds for the session.
    idleTimeoutSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The Amazon S3 location that stores information for the notebook.
    workingDirectory :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SessionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionConfiguration', 'sessionConfiguration_encryptionConfiguration' - Undocumented member.
--
-- 'executionRole', 'sessionConfiguration_executionRole' - The ARN of the execution role used for the session.
--
-- 'idleTimeoutSeconds', 'sessionConfiguration_idleTimeoutSeconds' - The idle timeout in seconds for the session.
--
-- 'workingDirectory', 'sessionConfiguration_workingDirectory' - The Amazon S3 location that stores information for the notebook.
newSessionConfiguration ::
  SessionConfiguration
newSessionConfiguration =
  SessionConfiguration'
    { encryptionConfiguration =
        Prelude.Nothing,
      executionRole = Prelude.Nothing,
      idleTimeoutSeconds = Prelude.Nothing,
      workingDirectory = Prelude.Nothing
    }

-- | Undocumented member.
sessionConfiguration_encryptionConfiguration :: Lens.Lens' SessionConfiguration (Prelude.Maybe EncryptionConfiguration)
sessionConfiguration_encryptionConfiguration = Lens.lens (\SessionConfiguration' {encryptionConfiguration} -> encryptionConfiguration) (\s@SessionConfiguration' {} a -> s {encryptionConfiguration = a} :: SessionConfiguration)

-- | The ARN of the execution role used for the session.
sessionConfiguration_executionRole :: Lens.Lens' SessionConfiguration (Prelude.Maybe Prelude.Text)
sessionConfiguration_executionRole = Lens.lens (\SessionConfiguration' {executionRole} -> executionRole) (\s@SessionConfiguration' {} a -> s {executionRole = a} :: SessionConfiguration)

-- | The idle timeout in seconds for the session.
sessionConfiguration_idleTimeoutSeconds :: Lens.Lens' SessionConfiguration (Prelude.Maybe Prelude.Integer)
sessionConfiguration_idleTimeoutSeconds = Lens.lens (\SessionConfiguration' {idleTimeoutSeconds} -> idleTimeoutSeconds) (\s@SessionConfiguration' {} a -> s {idleTimeoutSeconds = a} :: SessionConfiguration)

-- | The Amazon S3 location that stores information for the notebook.
sessionConfiguration_workingDirectory :: Lens.Lens' SessionConfiguration (Prelude.Maybe Prelude.Text)
sessionConfiguration_workingDirectory = Lens.lens (\SessionConfiguration' {workingDirectory} -> workingDirectory) (\s@SessionConfiguration' {} a -> s {workingDirectory = a} :: SessionConfiguration)

instance Data.FromJSON SessionConfiguration where
  parseJSON =
    Data.withObject
      "SessionConfiguration"
      ( \x ->
          SessionConfiguration'
            Prelude.<$> (x Data..:? "EncryptionConfiguration")
            Prelude.<*> (x Data..:? "ExecutionRole")
            Prelude.<*> (x Data..:? "IdleTimeoutSeconds")
            Prelude.<*> (x Data..:? "WorkingDirectory")
      )

instance Prelude.Hashable SessionConfiguration where
  hashWithSalt _salt SessionConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` encryptionConfiguration
      `Prelude.hashWithSalt` executionRole
      `Prelude.hashWithSalt` idleTimeoutSeconds
      `Prelude.hashWithSalt` workingDirectory

instance Prelude.NFData SessionConfiguration where
  rnf SessionConfiguration' {..} =
    Prelude.rnf encryptionConfiguration
      `Prelude.seq` Prelude.rnf executionRole
      `Prelude.seq` Prelude.rnf idleTimeoutSeconds
      `Prelude.seq` Prelude.rnf workingDirectory
