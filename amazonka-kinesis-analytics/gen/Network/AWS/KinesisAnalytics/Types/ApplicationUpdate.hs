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
-- Module      : Network.AWS.KinesisAnalytics.Types.ApplicationUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.ApplicationUpdate where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisAnalytics.Types.CloudWatchLoggingOptionUpdate
import Network.AWS.KinesisAnalytics.Types.InputUpdate
import Network.AWS.KinesisAnalytics.Types.OutputUpdate
import Network.AWS.KinesisAnalytics.Types.ReferenceDataSourceUpdate
import qualified Network.AWS.Lens as Lens

-- | Describes updates to apply to an existing Amazon Kinesis Analytics
-- application.
--
-- /See:/ 'newApplicationUpdate' smart constructor.
data ApplicationUpdate = ApplicationUpdate'
  { -- | Describes application reference data source updates.
    referenceDataSourceUpdates :: Core.Maybe [ReferenceDataSourceUpdate],
    -- | Describes application input configuration updates.
    inputUpdates :: Core.Maybe [InputUpdate],
    -- | Describes application CloudWatch logging option updates.
    cloudWatchLoggingOptionUpdates :: Core.Maybe [CloudWatchLoggingOptionUpdate],
    -- | Describes application code updates.
    applicationCodeUpdate :: Core.Maybe Core.Text,
    -- | Describes application output configuration updates.
    outputUpdates :: Core.Maybe [OutputUpdate]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApplicationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'referenceDataSourceUpdates', 'applicationUpdate_referenceDataSourceUpdates' - Describes application reference data source updates.
--
-- 'inputUpdates', 'applicationUpdate_inputUpdates' - Describes application input configuration updates.
--
-- 'cloudWatchLoggingOptionUpdates', 'applicationUpdate_cloudWatchLoggingOptionUpdates' - Describes application CloudWatch logging option updates.
--
-- 'applicationCodeUpdate', 'applicationUpdate_applicationCodeUpdate' - Describes application code updates.
--
-- 'outputUpdates', 'applicationUpdate_outputUpdates' - Describes application output configuration updates.
newApplicationUpdate ::
  ApplicationUpdate
newApplicationUpdate =
  ApplicationUpdate'
    { referenceDataSourceUpdates =
        Core.Nothing,
      inputUpdates = Core.Nothing,
      cloudWatchLoggingOptionUpdates = Core.Nothing,
      applicationCodeUpdate = Core.Nothing,
      outputUpdates = Core.Nothing
    }

-- | Describes application reference data source updates.
applicationUpdate_referenceDataSourceUpdates :: Lens.Lens' ApplicationUpdate (Core.Maybe [ReferenceDataSourceUpdate])
applicationUpdate_referenceDataSourceUpdates = Lens.lens (\ApplicationUpdate' {referenceDataSourceUpdates} -> referenceDataSourceUpdates) (\s@ApplicationUpdate' {} a -> s {referenceDataSourceUpdates = a} :: ApplicationUpdate) Core.. Lens.mapping Lens._Coerce

-- | Describes application input configuration updates.
applicationUpdate_inputUpdates :: Lens.Lens' ApplicationUpdate (Core.Maybe [InputUpdate])
applicationUpdate_inputUpdates = Lens.lens (\ApplicationUpdate' {inputUpdates} -> inputUpdates) (\s@ApplicationUpdate' {} a -> s {inputUpdates = a} :: ApplicationUpdate) Core.. Lens.mapping Lens._Coerce

-- | Describes application CloudWatch logging option updates.
applicationUpdate_cloudWatchLoggingOptionUpdates :: Lens.Lens' ApplicationUpdate (Core.Maybe [CloudWatchLoggingOptionUpdate])
applicationUpdate_cloudWatchLoggingOptionUpdates = Lens.lens (\ApplicationUpdate' {cloudWatchLoggingOptionUpdates} -> cloudWatchLoggingOptionUpdates) (\s@ApplicationUpdate' {} a -> s {cloudWatchLoggingOptionUpdates = a} :: ApplicationUpdate) Core.. Lens.mapping Lens._Coerce

-- | Describes application code updates.
applicationUpdate_applicationCodeUpdate :: Lens.Lens' ApplicationUpdate (Core.Maybe Core.Text)
applicationUpdate_applicationCodeUpdate = Lens.lens (\ApplicationUpdate' {applicationCodeUpdate} -> applicationCodeUpdate) (\s@ApplicationUpdate' {} a -> s {applicationCodeUpdate = a} :: ApplicationUpdate)

-- | Describes application output configuration updates.
applicationUpdate_outputUpdates :: Lens.Lens' ApplicationUpdate (Core.Maybe [OutputUpdate])
applicationUpdate_outputUpdates = Lens.lens (\ApplicationUpdate' {outputUpdates} -> outputUpdates) (\s@ApplicationUpdate' {} a -> s {outputUpdates = a} :: ApplicationUpdate) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable ApplicationUpdate

instance Core.NFData ApplicationUpdate

instance Core.ToJSON ApplicationUpdate where
  toJSON ApplicationUpdate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ReferenceDataSourceUpdates" Core..=)
              Core.<$> referenceDataSourceUpdates,
            ("InputUpdates" Core..=) Core.<$> inputUpdates,
            ("CloudWatchLoggingOptionUpdates" Core..=)
              Core.<$> cloudWatchLoggingOptionUpdates,
            ("ApplicationCodeUpdate" Core..=)
              Core.<$> applicationCodeUpdate,
            ("OutputUpdates" Core..=) Core.<$> outputUpdates
          ]
      )
