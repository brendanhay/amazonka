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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.WorkerConfigurationRevisionDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The description of the worker configuration revision.
--
-- /See:/ 'newWorkerConfigurationRevisionDescription' smart constructor.
data WorkerConfigurationRevisionDescription = WorkerConfigurationRevisionDescription'
  { -- | The time that the worker configuration was created.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The description of the worker configuration revision.
    description :: Prelude.Maybe Prelude.Text,
    -- | Base64 encoded contents of the connect-distributed.properties file.
    propertiesFileContent :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The description of a revision of the worker configuration.
    revision :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
-- 'description', 'workerConfigurationRevisionDescription_description' - The description of the worker configuration revision.
--
-- 'propertiesFileContent', 'workerConfigurationRevisionDescription_propertiesFileContent' - Base64 encoded contents of the connect-distributed.properties file.
--
-- 'revision', 'workerConfigurationRevisionDescription_revision' - The description of a revision of the worker configuration.
newWorkerConfigurationRevisionDescription ::
  WorkerConfigurationRevisionDescription
newWorkerConfigurationRevisionDescription =
  WorkerConfigurationRevisionDescription'
    { creationTime =
        Prelude.Nothing,
      description = Prelude.Nothing,
      propertiesFileContent =
        Prelude.Nothing,
      revision = Prelude.Nothing
    }

-- | The time that the worker configuration was created.
workerConfigurationRevisionDescription_creationTime :: Lens.Lens' WorkerConfigurationRevisionDescription (Prelude.Maybe Prelude.UTCTime)
workerConfigurationRevisionDescription_creationTime = Lens.lens (\WorkerConfigurationRevisionDescription' {creationTime} -> creationTime) (\s@WorkerConfigurationRevisionDescription' {} a -> s {creationTime = a} :: WorkerConfigurationRevisionDescription) Prelude.. Lens.mapping Data._Time

-- | The description of the worker configuration revision.
workerConfigurationRevisionDescription_description :: Lens.Lens' WorkerConfigurationRevisionDescription (Prelude.Maybe Prelude.Text)
workerConfigurationRevisionDescription_description = Lens.lens (\WorkerConfigurationRevisionDescription' {description} -> description) (\s@WorkerConfigurationRevisionDescription' {} a -> s {description = a} :: WorkerConfigurationRevisionDescription)

-- | Base64 encoded contents of the connect-distributed.properties file.
workerConfigurationRevisionDescription_propertiesFileContent :: Lens.Lens' WorkerConfigurationRevisionDescription (Prelude.Maybe Prelude.Text)
workerConfigurationRevisionDescription_propertiesFileContent = Lens.lens (\WorkerConfigurationRevisionDescription' {propertiesFileContent} -> propertiesFileContent) (\s@WorkerConfigurationRevisionDescription' {} a -> s {propertiesFileContent = a} :: WorkerConfigurationRevisionDescription) Prelude.. Lens.mapping Data._Sensitive

-- | The description of a revision of the worker configuration.
workerConfigurationRevisionDescription_revision :: Lens.Lens' WorkerConfigurationRevisionDescription (Prelude.Maybe Prelude.Integer)
workerConfigurationRevisionDescription_revision = Lens.lens (\WorkerConfigurationRevisionDescription' {revision} -> revision) (\s@WorkerConfigurationRevisionDescription' {} a -> s {revision = a} :: WorkerConfigurationRevisionDescription)

instance
  Data.FromJSON
    WorkerConfigurationRevisionDescription
  where
  parseJSON =
    Data.withObject
      "WorkerConfigurationRevisionDescription"
      ( \x ->
          WorkerConfigurationRevisionDescription'
            Prelude.<$> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "propertiesFileContent")
            Prelude.<*> (x Data..:? "revision")
      )

instance
  Prelude.Hashable
    WorkerConfigurationRevisionDescription
  where
  hashWithSalt
    _salt
    WorkerConfigurationRevisionDescription' {..} =
      _salt
        `Prelude.hashWithSalt` creationTime
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` propertiesFileContent
        `Prelude.hashWithSalt` revision

instance
  Prelude.NFData
    WorkerConfigurationRevisionDescription
  where
  rnf WorkerConfigurationRevisionDescription' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf propertiesFileContent
      `Prelude.seq` Prelude.rnf revision
