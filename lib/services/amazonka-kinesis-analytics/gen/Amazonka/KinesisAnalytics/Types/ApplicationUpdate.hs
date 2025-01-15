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
-- Module      : Amazonka.KinesisAnalytics.Types.ApplicationUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.ApplicationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalytics.Types.CloudWatchLoggingOptionUpdate
import Amazonka.KinesisAnalytics.Types.InputUpdate
import Amazonka.KinesisAnalytics.Types.OutputUpdate
import Amazonka.KinesisAnalytics.Types.ReferenceDataSourceUpdate
import qualified Amazonka.Prelude as Prelude

-- | Describes updates to apply to an existing Amazon Kinesis Analytics
-- application.
--
-- /See:/ 'newApplicationUpdate' smart constructor.
data ApplicationUpdate = ApplicationUpdate'
  { -- | Describes application code updates.
    applicationCodeUpdate :: Prelude.Maybe Prelude.Text,
    -- | Describes application CloudWatch logging option updates.
    cloudWatchLoggingOptionUpdates :: Prelude.Maybe [CloudWatchLoggingOptionUpdate],
    -- | Describes application input configuration updates.
    inputUpdates :: Prelude.Maybe [InputUpdate],
    -- | Describes application output configuration updates.
    outputUpdates :: Prelude.Maybe [OutputUpdate],
    -- | Describes application reference data source updates.
    referenceDataSourceUpdates :: Prelude.Maybe [ReferenceDataSourceUpdate]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationCodeUpdate', 'applicationUpdate_applicationCodeUpdate' - Describes application code updates.
--
-- 'cloudWatchLoggingOptionUpdates', 'applicationUpdate_cloudWatchLoggingOptionUpdates' - Describes application CloudWatch logging option updates.
--
-- 'inputUpdates', 'applicationUpdate_inputUpdates' - Describes application input configuration updates.
--
-- 'outputUpdates', 'applicationUpdate_outputUpdates' - Describes application output configuration updates.
--
-- 'referenceDataSourceUpdates', 'applicationUpdate_referenceDataSourceUpdates' - Describes application reference data source updates.
newApplicationUpdate ::
  ApplicationUpdate
newApplicationUpdate =
  ApplicationUpdate'
    { applicationCodeUpdate =
        Prelude.Nothing,
      cloudWatchLoggingOptionUpdates = Prelude.Nothing,
      inputUpdates = Prelude.Nothing,
      outputUpdates = Prelude.Nothing,
      referenceDataSourceUpdates = Prelude.Nothing
    }

-- | Describes application code updates.
applicationUpdate_applicationCodeUpdate :: Lens.Lens' ApplicationUpdate (Prelude.Maybe Prelude.Text)
applicationUpdate_applicationCodeUpdate = Lens.lens (\ApplicationUpdate' {applicationCodeUpdate} -> applicationCodeUpdate) (\s@ApplicationUpdate' {} a -> s {applicationCodeUpdate = a} :: ApplicationUpdate)

-- | Describes application CloudWatch logging option updates.
applicationUpdate_cloudWatchLoggingOptionUpdates :: Lens.Lens' ApplicationUpdate (Prelude.Maybe [CloudWatchLoggingOptionUpdate])
applicationUpdate_cloudWatchLoggingOptionUpdates = Lens.lens (\ApplicationUpdate' {cloudWatchLoggingOptionUpdates} -> cloudWatchLoggingOptionUpdates) (\s@ApplicationUpdate' {} a -> s {cloudWatchLoggingOptionUpdates = a} :: ApplicationUpdate) Prelude.. Lens.mapping Lens.coerced

-- | Describes application input configuration updates.
applicationUpdate_inputUpdates :: Lens.Lens' ApplicationUpdate (Prelude.Maybe [InputUpdate])
applicationUpdate_inputUpdates = Lens.lens (\ApplicationUpdate' {inputUpdates} -> inputUpdates) (\s@ApplicationUpdate' {} a -> s {inputUpdates = a} :: ApplicationUpdate) Prelude.. Lens.mapping Lens.coerced

-- | Describes application output configuration updates.
applicationUpdate_outputUpdates :: Lens.Lens' ApplicationUpdate (Prelude.Maybe [OutputUpdate])
applicationUpdate_outputUpdates = Lens.lens (\ApplicationUpdate' {outputUpdates} -> outputUpdates) (\s@ApplicationUpdate' {} a -> s {outputUpdates = a} :: ApplicationUpdate) Prelude.. Lens.mapping Lens.coerced

-- | Describes application reference data source updates.
applicationUpdate_referenceDataSourceUpdates :: Lens.Lens' ApplicationUpdate (Prelude.Maybe [ReferenceDataSourceUpdate])
applicationUpdate_referenceDataSourceUpdates = Lens.lens (\ApplicationUpdate' {referenceDataSourceUpdates} -> referenceDataSourceUpdates) (\s@ApplicationUpdate' {} a -> s {referenceDataSourceUpdates = a} :: ApplicationUpdate) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ApplicationUpdate where
  hashWithSalt _salt ApplicationUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` applicationCodeUpdate
      `Prelude.hashWithSalt` cloudWatchLoggingOptionUpdates
      `Prelude.hashWithSalt` inputUpdates
      `Prelude.hashWithSalt` outputUpdates
      `Prelude.hashWithSalt` referenceDataSourceUpdates

instance Prelude.NFData ApplicationUpdate where
  rnf ApplicationUpdate' {..} =
    Prelude.rnf applicationCodeUpdate `Prelude.seq`
      Prelude.rnf cloudWatchLoggingOptionUpdates `Prelude.seq`
        Prelude.rnf inputUpdates `Prelude.seq`
          Prelude.rnf outputUpdates `Prelude.seq`
            Prelude.rnf referenceDataSourceUpdates

instance Data.ToJSON ApplicationUpdate where
  toJSON ApplicationUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ApplicationCodeUpdate" Data..=)
              Prelude.<$> applicationCodeUpdate,
            ("CloudWatchLoggingOptionUpdates" Data..=)
              Prelude.<$> cloudWatchLoggingOptionUpdates,
            ("InputUpdates" Data..=) Prelude.<$> inputUpdates,
            ("OutputUpdates" Data..=) Prelude.<$> outputUpdates,
            ("ReferenceDataSourceUpdates" Data..=)
              Prelude.<$> referenceDataSourceUpdates
          ]
      )
