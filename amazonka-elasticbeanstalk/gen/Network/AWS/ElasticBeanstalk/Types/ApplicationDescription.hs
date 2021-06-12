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
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ApplicationDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types.ApplicationResourceLifecycleConfig
import qualified Network.AWS.Lens as Lens

-- | Describes the properties of an application.
--
-- /See:/ 'newApplicationDescription' smart constructor.
data ApplicationDescription = ApplicationDescription'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationArn :: Core.Maybe Core.Text,
    -- | The date when the application was created.
    dateCreated :: Core.Maybe Core.ISO8601,
    -- | The names of the versions for this application.
    versions :: Core.Maybe [Core.Text],
    -- | The date when the application was last modified.
    dateUpdated :: Core.Maybe Core.ISO8601,
    -- | The lifecycle settings for the application.
    resourceLifecycleConfig :: Core.Maybe ApplicationResourceLifecycleConfig,
    -- | User-defined description of the application.
    description :: Core.Maybe Core.Text,
    -- | The names of the configuration templates associated with this
    -- application.
    configurationTemplates :: Core.Maybe [Core.Text],
    -- | The name of the application.
    applicationName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApplicationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationArn', 'applicationDescription_applicationArn' - The Amazon Resource Name (ARN) of the application.
--
-- 'dateCreated', 'applicationDescription_dateCreated' - The date when the application was created.
--
-- 'versions', 'applicationDescription_versions' - The names of the versions for this application.
--
-- 'dateUpdated', 'applicationDescription_dateUpdated' - The date when the application was last modified.
--
-- 'resourceLifecycleConfig', 'applicationDescription_resourceLifecycleConfig' - The lifecycle settings for the application.
--
-- 'description', 'applicationDescription_description' - User-defined description of the application.
--
-- 'configurationTemplates', 'applicationDescription_configurationTemplates' - The names of the configuration templates associated with this
-- application.
--
-- 'applicationName', 'applicationDescription_applicationName' - The name of the application.
newApplicationDescription ::
  ApplicationDescription
newApplicationDescription =
  ApplicationDescription'
    { applicationArn =
        Core.Nothing,
      dateCreated = Core.Nothing,
      versions = Core.Nothing,
      dateUpdated = Core.Nothing,
      resourceLifecycleConfig = Core.Nothing,
      description = Core.Nothing,
      configurationTemplates = Core.Nothing,
      applicationName = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the application.
applicationDescription_applicationArn :: Lens.Lens' ApplicationDescription (Core.Maybe Core.Text)
applicationDescription_applicationArn = Lens.lens (\ApplicationDescription' {applicationArn} -> applicationArn) (\s@ApplicationDescription' {} a -> s {applicationArn = a} :: ApplicationDescription)

-- | The date when the application was created.
applicationDescription_dateCreated :: Lens.Lens' ApplicationDescription (Core.Maybe Core.UTCTime)
applicationDescription_dateCreated = Lens.lens (\ApplicationDescription' {dateCreated} -> dateCreated) (\s@ApplicationDescription' {} a -> s {dateCreated = a} :: ApplicationDescription) Core.. Lens.mapping Core._Time

-- | The names of the versions for this application.
applicationDescription_versions :: Lens.Lens' ApplicationDescription (Core.Maybe [Core.Text])
applicationDescription_versions = Lens.lens (\ApplicationDescription' {versions} -> versions) (\s@ApplicationDescription' {} a -> s {versions = a} :: ApplicationDescription) Core.. Lens.mapping Lens._Coerce

-- | The date when the application was last modified.
applicationDescription_dateUpdated :: Lens.Lens' ApplicationDescription (Core.Maybe Core.UTCTime)
applicationDescription_dateUpdated = Lens.lens (\ApplicationDescription' {dateUpdated} -> dateUpdated) (\s@ApplicationDescription' {} a -> s {dateUpdated = a} :: ApplicationDescription) Core.. Lens.mapping Core._Time

-- | The lifecycle settings for the application.
applicationDescription_resourceLifecycleConfig :: Lens.Lens' ApplicationDescription (Core.Maybe ApplicationResourceLifecycleConfig)
applicationDescription_resourceLifecycleConfig = Lens.lens (\ApplicationDescription' {resourceLifecycleConfig} -> resourceLifecycleConfig) (\s@ApplicationDescription' {} a -> s {resourceLifecycleConfig = a} :: ApplicationDescription)

-- | User-defined description of the application.
applicationDescription_description :: Lens.Lens' ApplicationDescription (Core.Maybe Core.Text)
applicationDescription_description = Lens.lens (\ApplicationDescription' {description} -> description) (\s@ApplicationDescription' {} a -> s {description = a} :: ApplicationDescription)

-- | The names of the configuration templates associated with this
-- application.
applicationDescription_configurationTemplates :: Lens.Lens' ApplicationDescription (Core.Maybe [Core.Text])
applicationDescription_configurationTemplates = Lens.lens (\ApplicationDescription' {configurationTemplates} -> configurationTemplates) (\s@ApplicationDescription' {} a -> s {configurationTemplates = a} :: ApplicationDescription) Core.. Lens.mapping Lens._Coerce

-- | The name of the application.
applicationDescription_applicationName :: Lens.Lens' ApplicationDescription (Core.Maybe Core.Text)
applicationDescription_applicationName = Lens.lens (\ApplicationDescription' {applicationName} -> applicationName) (\s@ApplicationDescription' {} a -> s {applicationName = a} :: ApplicationDescription)

instance Core.FromXML ApplicationDescription where
  parseXML x =
    ApplicationDescription'
      Core.<$> (x Core..@? "ApplicationArn")
      Core.<*> (x Core..@? "DateCreated")
      Core.<*> ( x Core..@? "Versions" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "DateUpdated")
      Core.<*> (x Core..@? "ResourceLifecycleConfig")
      Core.<*> (x Core..@? "Description")
      Core.<*> ( x Core..@? "ConfigurationTemplates"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "ApplicationName")

instance Core.Hashable ApplicationDescription

instance Core.NFData ApplicationDescription
