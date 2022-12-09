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
-- Module      : Amazonka.Forecast.Types.ExplainabilitySummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.ExplainabilitySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types.ExplainabilityConfig
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the Explainability properties used in the
-- ListExplainabilities operation. To get a complete set of properties,
-- call the DescribeExplainability operation, and provide the listed
-- @ExplainabilityArn@.
--
-- /See:/ 'newExplainabilitySummary' smart constructor.
data ExplainabilitySummary = ExplainabilitySummary'
  { -- | When the Explainability was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the Explainability.
    explainabilityArn :: Prelude.Maybe Prelude.Text,
    -- | The configuration settings that define the granularity of time series
    -- and time points for the Explainability.
    explainabilityConfig :: Prelude.Maybe ExplainabilityConfig,
    -- | The name of the Explainability.
    explainabilityName :: Prelude.Maybe Prelude.Text,
    -- | The last time the resource was modified. The timestamp depends on the
    -- status of the job:
    --
    -- -   @CREATE_PENDING@ - The @CreationTime@.
    --
    -- -   @CREATE_IN_PROGRESS@ - The current timestamp.
    --
    -- -   @CREATE_STOPPING@ - The current timestamp.
    --
    -- -   @CREATE_STOPPED@ - When the job stopped.
    --
    -- -   @ACTIVE@ or @CREATE_FAILED@ - When the job finished or failed.
    lastModificationTime :: Prelude.Maybe Data.POSIX,
    -- | Information about any errors that may have occurred during the
    -- Explainability creation process.
    message :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Predictor or Forecast used to
    -- create the Explainability.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the Explainability. States include:
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
    --
    -- -   @CREATE_STOPPING@, @CREATE_STOPPED@
    --
    -- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExplainabilitySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'explainabilitySummary_creationTime' - When the Explainability was created.
--
-- 'explainabilityArn', 'explainabilitySummary_explainabilityArn' - The Amazon Resource Name (ARN) of the Explainability.
--
-- 'explainabilityConfig', 'explainabilitySummary_explainabilityConfig' - The configuration settings that define the granularity of time series
-- and time points for the Explainability.
--
-- 'explainabilityName', 'explainabilitySummary_explainabilityName' - The name of the Explainability.
--
-- 'lastModificationTime', 'explainabilitySummary_lastModificationTime' - The last time the resource was modified. The timestamp depends on the
-- status of the job:
--
-- -   @CREATE_PENDING@ - The @CreationTime@.
--
-- -   @CREATE_IN_PROGRESS@ - The current timestamp.
--
-- -   @CREATE_STOPPING@ - The current timestamp.
--
-- -   @CREATE_STOPPED@ - When the job stopped.
--
-- -   @ACTIVE@ or @CREATE_FAILED@ - When the job finished or failed.
--
-- 'message', 'explainabilitySummary_message' - Information about any errors that may have occurred during the
-- Explainability creation process.
--
-- 'resourceArn', 'explainabilitySummary_resourceArn' - The Amazon Resource Name (ARN) of the Predictor or Forecast used to
-- create the Explainability.
--
-- 'status', 'explainabilitySummary_status' - The status of the Explainability. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
newExplainabilitySummary ::
  ExplainabilitySummary
newExplainabilitySummary =
  ExplainabilitySummary'
    { creationTime =
        Prelude.Nothing,
      explainabilityArn = Prelude.Nothing,
      explainabilityConfig = Prelude.Nothing,
      explainabilityName = Prelude.Nothing,
      lastModificationTime = Prelude.Nothing,
      message = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | When the Explainability was created.
explainabilitySummary_creationTime :: Lens.Lens' ExplainabilitySummary (Prelude.Maybe Prelude.UTCTime)
explainabilitySummary_creationTime = Lens.lens (\ExplainabilitySummary' {creationTime} -> creationTime) (\s@ExplainabilitySummary' {} a -> s {creationTime = a} :: ExplainabilitySummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the Explainability.
explainabilitySummary_explainabilityArn :: Lens.Lens' ExplainabilitySummary (Prelude.Maybe Prelude.Text)
explainabilitySummary_explainabilityArn = Lens.lens (\ExplainabilitySummary' {explainabilityArn} -> explainabilityArn) (\s@ExplainabilitySummary' {} a -> s {explainabilityArn = a} :: ExplainabilitySummary)

-- | The configuration settings that define the granularity of time series
-- and time points for the Explainability.
explainabilitySummary_explainabilityConfig :: Lens.Lens' ExplainabilitySummary (Prelude.Maybe ExplainabilityConfig)
explainabilitySummary_explainabilityConfig = Lens.lens (\ExplainabilitySummary' {explainabilityConfig} -> explainabilityConfig) (\s@ExplainabilitySummary' {} a -> s {explainabilityConfig = a} :: ExplainabilitySummary)

-- | The name of the Explainability.
explainabilitySummary_explainabilityName :: Lens.Lens' ExplainabilitySummary (Prelude.Maybe Prelude.Text)
explainabilitySummary_explainabilityName = Lens.lens (\ExplainabilitySummary' {explainabilityName} -> explainabilityName) (\s@ExplainabilitySummary' {} a -> s {explainabilityName = a} :: ExplainabilitySummary)

-- | The last time the resource was modified. The timestamp depends on the
-- status of the job:
--
-- -   @CREATE_PENDING@ - The @CreationTime@.
--
-- -   @CREATE_IN_PROGRESS@ - The current timestamp.
--
-- -   @CREATE_STOPPING@ - The current timestamp.
--
-- -   @CREATE_STOPPED@ - When the job stopped.
--
-- -   @ACTIVE@ or @CREATE_FAILED@ - When the job finished or failed.
explainabilitySummary_lastModificationTime :: Lens.Lens' ExplainabilitySummary (Prelude.Maybe Prelude.UTCTime)
explainabilitySummary_lastModificationTime = Lens.lens (\ExplainabilitySummary' {lastModificationTime} -> lastModificationTime) (\s@ExplainabilitySummary' {} a -> s {lastModificationTime = a} :: ExplainabilitySummary) Prelude.. Lens.mapping Data._Time

-- | Information about any errors that may have occurred during the
-- Explainability creation process.
explainabilitySummary_message :: Lens.Lens' ExplainabilitySummary (Prelude.Maybe Prelude.Text)
explainabilitySummary_message = Lens.lens (\ExplainabilitySummary' {message} -> message) (\s@ExplainabilitySummary' {} a -> s {message = a} :: ExplainabilitySummary)

-- | The Amazon Resource Name (ARN) of the Predictor or Forecast used to
-- create the Explainability.
explainabilitySummary_resourceArn :: Lens.Lens' ExplainabilitySummary (Prelude.Maybe Prelude.Text)
explainabilitySummary_resourceArn = Lens.lens (\ExplainabilitySummary' {resourceArn} -> resourceArn) (\s@ExplainabilitySummary' {} a -> s {resourceArn = a} :: ExplainabilitySummary)

-- | The status of the Explainability. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
explainabilitySummary_status :: Lens.Lens' ExplainabilitySummary (Prelude.Maybe Prelude.Text)
explainabilitySummary_status = Lens.lens (\ExplainabilitySummary' {status} -> status) (\s@ExplainabilitySummary' {} a -> s {status = a} :: ExplainabilitySummary)

instance Data.FromJSON ExplainabilitySummary where
  parseJSON =
    Data.withObject
      "ExplainabilitySummary"
      ( \x ->
          ExplainabilitySummary'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "ExplainabilityArn")
            Prelude.<*> (x Data..:? "ExplainabilityConfig")
            Prelude.<*> (x Data..:? "ExplainabilityName")
            Prelude.<*> (x Data..:? "LastModificationTime")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "ResourceArn")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable ExplainabilitySummary where
  hashWithSalt _salt ExplainabilitySummary' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` explainabilityArn
      `Prelude.hashWithSalt` explainabilityConfig
      `Prelude.hashWithSalt` explainabilityName
      `Prelude.hashWithSalt` lastModificationTime
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData ExplainabilitySummary where
  rnf ExplainabilitySummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf explainabilityArn
      `Prelude.seq` Prelude.rnf explainabilityConfig
      `Prelude.seq` Prelude.rnf explainabilityName
      `Prelude.seq` Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf status
