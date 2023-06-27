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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.MediaInsightsPipelineConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.MediaInsightsPipelineConfiguration where

import Amazonka.ChimeSdkMediaPipelines.Types.MediaInsightsPipelineConfigurationElement
import Amazonka.ChimeSdkMediaPipelines.Types.RealTimeAlertConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the configuration settings for a media
-- insights pipeline.
--
-- /See:/ 'newMediaInsightsPipelineConfiguration' smart constructor.
data MediaInsightsPipelineConfiguration = MediaInsightsPipelineConfiguration'
  { -- | The time at which the configuration was created.
    createdTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | The elements in the configuration.
    elements :: Prelude.Maybe [MediaInsightsPipelineConfigurationElement],
    -- | The ARN of the configuration.
    mediaInsightsPipelineConfigurationArn :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the configuration.
    mediaInsightsPipelineConfigurationId :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration.
    mediaInsightsPipelineConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | Lists the rules that trigger a real-time alert.
    realTimeAlertConfiguration :: Prelude.Maybe RealTimeAlertConfiguration,
    -- | The ARN of the role used by the service to access Amazon Web Services
    -- resources.
    resourceAccessRoleArn :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The time at which the configuration was last updated.
    updatedTimestamp :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MediaInsightsPipelineConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'mediaInsightsPipelineConfiguration_createdTimestamp' - The time at which the configuration was created.
--
-- 'elements', 'mediaInsightsPipelineConfiguration_elements' - The elements in the configuration.
--
-- 'mediaInsightsPipelineConfigurationArn', 'mediaInsightsPipelineConfiguration_mediaInsightsPipelineConfigurationArn' - The ARN of the configuration.
--
-- 'mediaInsightsPipelineConfigurationId', 'mediaInsightsPipelineConfiguration_mediaInsightsPipelineConfigurationId' - The ID of the configuration.
--
-- 'mediaInsightsPipelineConfigurationName', 'mediaInsightsPipelineConfiguration_mediaInsightsPipelineConfigurationName' - The name of the configuration.
--
-- 'realTimeAlertConfiguration', 'mediaInsightsPipelineConfiguration_realTimeAlertConfiguration' - Lists the rules that trigger a real-time alert.
--
-- 'resourceAccessRoleArn', 'mediaInsightsPipelineConfiguration_resourceAccessRoleArn' - The ARN of the role used by the service to access Amazon Web Services
-- resources.
--
-- 'updatedTimestamp', 'mediaInsightsPipelineConfiguration_updatedTimestamp' - The time at which the configuration was last updated.
newMediaInsightsPipelineConfiguration ::
  MediaInsightsPipelineConfiguration
newMediaInsightsPipelineConfiguration =
  MediaInsightsPipelineConfiguration'
    { createdTimestamp =
        Prelude.Nothing,
      elements = Prelude.Nothing,
      mediaInsightsPipelineConfigurationArn =
        Prelude.Nothing,
      mediaInsightsPipelineConfigurationId =
        Prelude.Nothing,
      mediaInsightsPipelineConfigurationName =
        Prelude.Nothing,
      realTimeAlertConfiguration =
        Prelude.Nothing,
      resourceAccessRoleArn = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing
    }

-- | The time at which the configuration was created.
mediaInsightsPipelineConfiguration_createdTimestamp :: Lens.Lens' MediaInsightsPipelineConfiguration (Prelude.Maybe Prelude.UTCTime)
mediaInsightsPipelineConfiguration_createdTimestamp = Lens.lens (\MediaInsightsPipelineConfiguration' {createdTimestamp} -> createdTimestamp) (\s@MediaInsightsPipelineConfiguration' {} a -> s {createdTimestamp = a} :: MediaInsightsPipelineConfiguration) Prelude.. Lens.mapping Data._Time

-- | The elements in the configuration.
mediaInsightsPipelineConfiguration_elements :: Lens.Lens' MediaInsightsPipelineConfiguration (Prelude.Maybe [MediaInsightsPipelineConfigurationElement])
mediaInsightsPipelineConfiguration_elements = Lens.lens (\MediaInsightsPipelineConfiguration' {elements} -> elements) (\s@MediaInsightsPipelineConfiguration' {} a -> s {elements = a} :: MediaInsightsPipelineConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the configuration.
mediaInsightsPipelineConfiguration_mediaInsightsPipelineConfigurationArn :: Lens.Lens' MediaInsightsPipelineConfiguration (Prelude.Maybe Prelude.Text)
mediaInsightsPipelineConfiguration_mediaInsightsPipelineConfigurationArn = Lens.lens (\MediaInsightsPipelineConfiguration' {mediaInsightsPipelineConfigurationArn} -> mediaInsightsPipelineConfigurationArn) (\s@MediaInsightsPipelineConfiguration' {} a -> s {mediaInsightsPipelineConfigurationArn = a} :: MediaInsightsPipelineConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the configuration.
mediaInsightsPipelineConfiguration_mediaInsightsPipelineConfigurationId :: Lens.Lens' MediaInsightsPipelineConfiguration (Prelude.Maybe Prelude.Text)
mediaInsightsPipelineConfiguration_mediaInsightsPipelineConfigurationId = Lens.lens (\MediaInsightsPipelineConfiguration' {mediaInsightsPipelineConfigurationId} -> mediaInsightsPipelineConfigurationId) (\s@MediaInsightsPipelineConfiguration' {} a -> s {mediaInsightsPipelineConfigurationId = a} :: MediaInsightsPipelineConfiguration)

-- | The name of the configuration.
mediaInsightsPipelineConfiguration_mediaInsightsPipelineConfigurationName :: Lens.Lens' MediaInsightsPipelineConfiguration (Prelude.Maybe Prelude.Text)
mediaInsightsPipelineConfiguration_mediaInsightsPipelineConfigurationName = Lens.lens (\MediaInsightsPipelineConfiguration' {mediaInsightsPipelineConfigurationName} -> mediaInsightsPipelineConfigurationName) (\s@MediaInsightsPipelineConfiguration' {} a -> s {mediaInsightsPipelineConfigurationName = a} :: MediaInsightsPipelineConfiguration)

-- | Lists the rules that trigger a real-time alert.
mediaInsightsPipelineConfiguration_realTimeAlertConfiguration :: Lens.Lens' MediaInsightsPipelineConfiguration (Prelude.Maybe RealTimeAlertConfiguration)
mediaInsightsPipelineConfiguration_realTimeAlertConfiguration = Lens.lens (\MediaInsightsPipelineConfiguration' {realTimeAlertConfiguration} -> realTimeAlertConfiguration) (\s@MediaInsightsPipelineConfiguration' {} a -> s {realTimeAlertConfiguration = a} :: MediaInsightsPipelineConfiguration)

-- | The ARN of the role used by the service to access Amazon Web Services
-- resources.
mediaInsightsPipelineConfiguration_resourceAccessRoleArn :: Lens.Lens' MediaInsightsPipelineConfiguration (Prelude.Maybe Prelude.Text)
mediaInsightsPipelineConfiguration_resourceAccessRoleArn = Lens.lens (\MediaInsightsPipelineConfiguration' {resourceAccessRoleArn} -> resourceAccessRoleArn) (\s@MediaInsightsPipelineConfiguration' {} a -> s {resourceAccessRoleArn = a} :: MediaInsightsPipelineConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | The time at which the configuration was last updated.
mediaInsightsPipelineConfiguration_updatedTimestamp :: Lens.Lens' MediaInsightsPipelineConfiguration (Prelude.Maybe Prelude.UTCTime)
mediaInsightsPipelineConfiguration_updatedTimestamp = Lens.lens (\MediaInsightsPipelineConfiguration' {updatedTimestamp} -> updatedTimestamp) (\s@MediaInsightsPipelineConfiguration' {} a -> s {updatedTimestamp = a} :: MediaInsightsPipelineConfiguration) Prelude.. Lens.mapping Data._Time

instance
  Data.FromJSON
    MediaInsightsPipelineConfiguration
  where
  parseJSON =
    Data.withObject
      "MediaInsightsPipelineConfiguration"
      ( \x ->
          MediaInsightsPipelineConfiguration'
            Prelude.<$> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "Elements" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "MediaInsightsPipelineConfigurationArn")
            Prelude.<*> (x Data..:? "MediaInsightsPipelineConfigurationId")
            Prelude.<*> (x Data..:? "MediaInsightsPipelineConfigurationName")
            Prelude.<*> (x Data..:? "RealTimeAlertConfiguration")
            Prelude.<*> (x Data..:? "ResourceAccessRoleArn")
            Prelude.<*> (x Data..:? "UpdatedTimestamp")
      )

instance
  Prelude.Hashable
    MediaInsightsPipelineConfiguration
  where
  hashWithSalt
    _salt
    MediaInsightsPipelineConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` createdTimestamp
        `Prelude.hashWithSalt` elements
        `Prelude.hashWithSalt` mediaInsightsPipelineConfigurationArn
        `Prelude.hashWithSalt` mediaInsightsPipelineConfigurationId
        `Prelude.hashWithSalt` mediaInsightsPipelineConfigurationName
        `Prelude.hashWithSalt` realTimeAlertConfiguration
        `Prelude.hashWithSalt` resourceAccessRoleArn
        `Prelude.hashWithSalt` updatedTimestamp

instance
  Prelude.NFData
    MediaInsightsPipelineConfiguration
  where
  rnf MediaInsightsPipelineConfiguration' {..} =
    Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf elements
      `Prelude.seq` Prelude.rnf mediaInsightsPipelineConfigurationArn
      `Prelude.seq` Prelude.rnf mediaInsightsPipelineConfigurationId
      `Prelude.seq` Prelude.rnf mediaInsightsPipelineConfigurationName
      `Prelude.seq` Prelude.rnf realTimeAlertConfiguration
      `Prelude.seq` Prelude.rnf resourceAccessRoleArn
      `Prelude.seq` Prelude.rnf updatedTimestamp
