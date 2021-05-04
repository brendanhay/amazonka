{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ElasticBeanstalk.Types.ApplicationResourceLifecycleConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the properties of an application.
--
-- /See:/ 'newApplicationDescription' smart constructor.
data ApplicationDescription = ApplicationDescription'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationArn :: Prelude.Maybe Prelude.Text,
    -- | The date when the application was created.
    dateCreated :: Prelude.Maybe Prelude.ISO8601,
    -- | The names of the versions for this application.
    versions :: Prelude.Maybe [Prelude.Text],
    -- | The date when the application was last modified.
    dateUpdated :: Prelude.Maybe Prelude.ISO8601,
    -- | The lifecycle settings for the application.
    resourceLifecycleConfig :: Prelude.Maybe ApplicationResourceLifecycleConfig,
    -- | User-defined description of the application.
    description :: Prelude.Maybe Prelude.Text,
    -- | The names of the configuration templates associated with this
    -- application.
    configurationTemplates :: Prelude.Maybe [Prelude.Text],
    -- | The name of the application.
    applicationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      versions = Prelude.Nothing,
      dateUpdated = Prelude.Nothing,
      resourceLifecycleConfig = Prelude.Nothing,
      description = Prelude.Nothing,
      configurationTemplates = Prelude.Nothing,
      applicationName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the application.
applicationDescription_applicationArn :: Lens.Lens' ApplicationDescription (Prelude.Maybe Prelude.Text)
applicationDescription_applicationArn = Lens.lens (\ApplicationDescription' {applicationArn} -> applicationArn) (\s@ApplicationDescription' {} a -> s {applicationArn = a} :: ApplicationDescription)

-- | The date when the application was created.
applicationDescription_dateCreated :: Lens.Lens' ApplicationDescription (Prelude.Maybe Prelude.UTCTime)
applicationDescription_dateCreated = Lens.lens (\ApplicationDescription' {dateCreated} -> dateCreated) (\s@ApplicationDescription' {} a -> s {dateCreated = a} :: ApplicationDescription) Prelude.. Lens.mapping Prelude._Time

-- | The names of the versions for this application.
applicationDescription_versions :: Lens.Lens' ApplicationDescription (Prelude.Maybe [Prelude.Text])
applicationDescription_versions = Lens.lens (\ApplicationDescription' {versions} -> versions) (\s@ApplicationDescription' {} a -> s {versions = a} :: ApplicationDescription) Prelude.. Lens.mapping Prelude._Coerce

-- | The date when the application was last modified.
applicationDescription_dateUpdated :: Lens.Lens' ApplicationDescription (Prelude.Maybe Prelude.UTCTime)
applicationDescription_dateUpdated = Lens.lens (\ApplicationDescription' {dateUpdated} -> dateUpdated) (\s@ApplicationDescription' {} a -> s {dateUpdated = a} :: ApplicationDescription) Prelude.. Lens.mapping Prelude._Time

-- | The lifecycle settings for the application.
applicationDescription_resourceLifecycleConfig :: Lens.Lens' ApplicationDescription (Prelude.Maybe ApplicationResourceLifecycleConfig)
applicationDescription_resourceLifecycleConfig = Lens.lens (\ApplicationDescription' {resourceLifecycleConfig} -> resourceLifecycleConfig) (\s@ApplicationDescription' {} a -> s {resourceLifecycleConfig = a} :: ApplicationDescription)

-- | User-defined description of the application.
applicationDescription_description :: Lens.Lens' ApplicationDescription (Prelude.Maybe Prelude.Text)
applicationDescription_description = Lens.lens (\ApplicationDescription' {description} -> description) (\s@ApplicationDescription' {} a -> s {description = a} :: ApplicationDescription)

-- | The names of the configuration templates associated with this
-- application.
applicationDescription_configurationTemplates :: Lens.Lens' ApplicationDescription (Prelude.Maybe [Prelude.Text])
applicationDescription_configurationTemplates = Lens.lens (\ApplicationDescription' {configurationTemplates} -> configurationTemplates) (\s@ApplicationDescription' {} a -> s {configurationTemplates = a} :: ApplicationDescription) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the application.
applicationDescription_applicationName :: Lens.Lens' ApplicationDescription (Prelude.Maybe Prelude.Text)
applicationDescription_applicationName = Lens.lens (\ApplicationDescription' {applicationName} -> applicationName) (\s@ApplicationDescription' {} a -> s {applicationName = a} :: ApplicationDescription)

instance Prelude.FromXML ApplicationDescription where
  parseXML x =
    ApplicationDescription'
      Prelude.<$> (x Prelude..@? "ApplicationArn")
      Prelude.<*> (x Prelude..@? "DateCreated")
      Prelude.<*> ( x Prelude..@? "Versions" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "DateUpdated")
      Prelude.<*> (x Prelude..@? "ResourceLifecycleConfig")
      Prelude.<*> (x Prelude..@? "Description")
      Prelude.<*> ( x Prelude..@? "ConfigurationTemplates"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "ApplicationName")

instance Prelude.Hashable ApplicationDescription

instance Prelude.NFData ApplicationDescription
