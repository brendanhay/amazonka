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
-- Module      : Amazonka.KafkaConnect.Types.WorkerConfigurationRevisionDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.WorkerConfigurationRevisionDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The description of the worker configuration revision.
--
-- /See:/ 'newWorkerConfigurationRevisionDescription' smart constructor.
data WorkerConfigurationRevisionDescription = WorkerConfigurationRevisionDescription'
  { -- | The time that the worker configuration was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The description of a revision of the worker configuration.
    revision :: Prelude.Maybe Prelude.Integer,
    -- | Base64 encoded contents of the connect-distributed.properties file.
    propertiesFileContent :: Prelude.Maybe Prelude.Text,
    -- | The description of the worker configuration revision.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkerConfigurationRevisionDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'workerConfigurationRevisionDescription_creationTime' - The time that the worker configuration was created.
--
-- 'revision', 'workerConfigurationRevisionDescription_revision' - The description of a revision of the worker configuration.
--
-- 'propertiesFileContent', 'workerConfigurationRevisionDescription_propertiesFileContent' - Base64 encoded contents of the connect-distributed.properties file.
--
-- 'description', 'workerConfigurationRevisionDescription_description' - The description of the worker configuration revision.
newWorkerConfigurationRevisionDescription ::
  WorkerConfigurationRevisionDescription
newWorkerConfigurationRevisionDescription =
  WorkerConfigurationRevisionDescription'
    { creationTime =
        Prelude.Nothing,
      revision = Prelude.Nothing,
      propertiesFileContent =
        Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The time that the worker configuration was created.
workerConfigurationRevisionDescription_creationTime :: Lens.Lens' WorkerConfigurationRevisionDescription (Prelude.Maybe Prelude.UTCTime)
workerConfigurationRevisionDescription_creationTime = Lens.lens (\WorkerConfigurationRevisionDescription' {creationTime} -> creationTime) (\s@WorkerConfigurationRevisionDescription' {} a -> s {creationTime = a} :: WorkerConfigurationRevisionDescription) Prelude.. Lens.mapping Core._Time

-- | The description of a revision of the worker configuration.
workerConfigurationRevisionDescription_revision :: Lens.Lens' WorkerConfigurationRevisionDescription (Prelude.Maybe Prelude.Integer)
workerConfigurationRevisionDescription_revision = Lens.lens (\WorkerConfigurationRevisionDescription' {revision} -> revision) (\s@WorkerConfigurationRevisionDescription' {} a -> s {revision = a} :: WorkerConfigurationRevisionDescription)

-- | Base64 encoded contents of the connect-distributed.properties file.
workerConfigurationRevisionDescription_propertiesFileContent :: Lens.Lens' WorkerConfigurationRevisionDescription (Prelude.Maybe Prelude.Text)
workerConfigurationRevisionDescription_propertiesFileContent = Lens.lens (\WorkerConfigurationRevisionDescription' {propertiesFileContent} -> propertiesFileContent) (\s@WorkerConfigurationRevisionDescription' {} a -> s {propertiesFileContent = a} :: WorkerConfigurationRevisionDescription)

-- | The description of the worker configuration revision.
workerConfigurationRevisionDescription_description :: Lens.Lens' WorkerConfigurationRevisionDescription (Prelude.Maybe Prelude.Text)
workerConfigurationRevisionDescription_description = Lens.lens (\WorkerConfigurationRevisionDescription' {description} -> description) (\s@WorkerConfigurationRevisionDescription' {} a -> s {description = a} :: WorkerConfigurationRevisionDescription)

instance
  Core.FromJSON
    WorkerConfigurationRevisionDescription
  where
  parseJSON =
    Core.withObject
      "WorkerConfigurationRevisionDescription"
      ( \x ->
          WorkerConfigurationRevisionDescription'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "revision")
            Prelude.<*> (x Core..:? "propertiesFileContent")
            Prelude.<*> (x Core..:? "description")
      )

instance
  Prelude.Hashable
    WorkerConfigurationRevisionDescription

instance
  Prelude.NFData
    WorkerConfigurationRevisionDescription
