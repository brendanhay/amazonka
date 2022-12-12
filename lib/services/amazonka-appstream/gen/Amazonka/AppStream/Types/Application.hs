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
-- Module      : Amazonka.AppStream.Types.Application
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.Application where

import Amazonka.AppStream.Types.PlatformType
import Amazonka.AppStream.Types.S3Location
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an application in the application catalog.
--
-- /See:/ 'newApplication' smart constructor.
data Application = Application'
  { -- | The app block ARN of the application.
    appBlockArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the application.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the application was created within the app block.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the application.
    description :: Prelude.Maybe Prelude.Text,
    -- | The application name to display.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | If there is a problem, the application can be disabled after image
    -- creation.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The S3 location of the application icon.
    iconS3Location :: Prelude.Maybe S3Location,
    -- | The URL for the application icon. This URL might be time-limited.
    iconURL :: Prelude.Maybe Prelude.Text,
    -- | The instance families for the application.
    instanceFamilies :: Prelude.Maybe [Prelude.Text],
    -- | The arguments that are passed to the application at launch.
    launchParameters :: Prelude.Maybe Prelude.Text,
    -- | The path to the application executable in the instance.
    launchPath :: Prelude.Maybe Prelude.Text,
    -- | Additional attributes that describe the application.
    metadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The platforms on which the application can run.
    platforms :: Prelude.Maybe [PlatformType],
    -- | The working directory for the application.
    workingDirectory :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Application' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appBlockArn', 'application_appBlockArn' - The app block ARN of the application.
--
-- 'arn', 'application_arn' - The ARN of the application.
--
-- 'createdTime', 'application_createdTime' - The time at which the application was created within the app block.
--
-- 'description', 'application_description' - The description of the application.
--
-- 'displayName', 'application_displayName' - The application name to display.
--
-- 'enabled', 'application_enabled' - If there is a problem, the application can be disabled after image
-- creation.
--
-- 'iconS3Location', 'application_iconS3Location' - The S3 location of the application icon.
--
-- 'iconURL', 'application_iconURL' - The URL for the application icon. This URL might be time-limited.
--
-- 'instanceFamilies', 'application_instanceFamilies' - The instance families for the application.
--
-- 'launchParameters', 'application_launchParameters' - The arguments that are passed to the application at launch.
--
-- 'launchPath', 'application_launchPath' - The path to the application executable in the instance.
--
-- 'metadata', 'application_metadata' - Additional attributes that describe the application.
--
-- 'name', 'application_name' - The name of the application.
--
-- 'platforms', 'application_platforms' - The platforms on which the application can run.
--
-- 'workingDirectory', 'application_workingDirectory' - The working directory for the application.
newApplication ::
  Application
newApplication =
  Application'
    { appBlockArn = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      description = Prelude.Nothing,
      displayName = Prelude.Nothing,
      enabled = Prelude.Nothing,
      iconS3Location = Prelude.Nothing,
      iconURL = Prelude.Nothing,
      instanceFamilies = Prelude.Nothing,
      launchParameters = Prelude.Nothing,
      launchPath = Prelude.Nothing,
      metadata = Prelude.Nothing,
      name = Prelude.Nothing,
      platforms = Prelude.Nothing,
      workingDirectory = Prelude.Nothing
    }

-- | The app block ARN of the application.
application_appBlockArn :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_appBlockArn = Lens.lens (\Application' {appBlockArn} -> appBlockArn) (\s@Application' {} a -> s {appBlockArn = a} :: Application)

-- | The ARN of the application.
application_arn :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_arn = Lens.lens (\Application' {arn} -> arn) (\s@Application' {} a -> s {arn = a} :: Application)

-- | The time at which the application was created within the app block.
application_createdTime :: Lens.Lens' Application (Prelude.Maybe Prelude.UTCTime)
application_createdTime = Lens.lens (\Application' {createdTime} -> createdTime) (\s@Application' {} a -> s {createdTime = a} :: Application) Prelude.. Lens.mapping Data._Time

-- | The description of the application.
application_description :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_description = Lens.lens (\Application' {description} -> description) (\s@Application' {} a -> s {description = a} :: Application)

-- | The application name to display.
application_displayName :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_displayName = Lens.lens (\Application' {displayName} -> displayName) (\s@Application' {} a -> s {displayName = a} :: Application)

-- | If there is a problem, the application can be disabled after image
-- creation.
application_enabled :: Lens.Lens' Application (Prelude.Maybe Prelude.Bool)
application_enabled = Lens.lens (\Application' {enabled} -> enabled) (\s@Application' {} a -> s {enabled = a} :: Application)

-- | The S3 location of the application icon.
application_iconS3Location :: Lens.Lens' Application (Prelude.Maybe S3Location)
application_iconS3Location = Lens.lens (\Application' {iconS3Location} -> iconS3Location) (\s@Application' {} a -> s {iconS3Location = a} :: Application)

-- | The URL for the application icon. This URL might be time-limited.
application_iconURL :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_iconURL = Lens.lens (\Application' {iconURL} -> iconURL) (\s@Application' {} a -> s {iconURL = a} :: Application)

-- | The instance families for the application.
application_instanceFamilies :: Lens.Lens' Application (Prelude.Maybe [Prelude.Text])
application_instanceFamilies = Lens.lens (\Application' {instanceFamilies} -> instanceFamilies) (\s@Application' {} a -> s {instanceFamilies = a} :: Application) Prelude.. Lens.mapping Lens.coerced

-- | The arguments that are passed to the application at launch.
application_launchParameters :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_launchParameters = Lens.lens (\Application' {launchParameters} -> launchParameters) (\s@Application' {} a -> s {launchParameters = a} :: Application)

-- | The path to the application executable in the instance.
application_launchPath :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_launchPath = Lens.lens (\Application' {launchPath} -> launchPath) (\s@Application' {} a -> s {launchPath = a} :: Application)

-- | Additional attributes that describe the application.
application_metadata :: Lens.Lens' Application (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
application_metadata = Lens.lens (\Application' {metadata} -> metadata) (\s@Application' {} a -> s {metadata = a} :: Application) Prelude.. Lens.mapping Lens.coerced

-- | The name of the application.
application_name :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_name = Lens.lens (\Application' {name} -> name) (\s@Application' {} a -> s {name = a} :: Application)

-- | The platforms on which the application can run.
application_platforms :: Lens.Lens' Application (Prelude.Maybe [PlatformType])
application_platforms = Lens.lens (\Application' {platforms} -> platforms) (\s@Application' {} a -> s {platforms = a} :: Application) Prelude.. Lens.mapping Lens.coerced

-- | The working directory for the application.
application_workingDirectory :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_workingDirectory = Lens.lens (\Application' {workingDirectory} -> workingDirectory) (\s@Application' {} a -> s {workingDirectory = a} :: Application)

instance Data.FromJSON Application where
  parseJSON =
    Data.withObject
      "Application"
      ( \x ->
          Application'
            Prelude.<$> (x Data..:? "AppBlockArn")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "DisplayName")
            Prelude.<*> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "IconS3Location")
            Prelude.<*> (x Data..:? "IconURL")
            Prelude.<*> ( x Data..:? "InstanceFamilies"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "LaunchParameters")
            Prelude.<*> (x Data..:? "LaunchPath")
            Prelude.<*> (x Data..:? "Metadata" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Platforms" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "WorkingDirectory")
      )

instance Prelude.Hashable Application where
  hashWithSalt _salt Application' {..} =
    _salt `Prelude.hashWithSalt` appBlockArn
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` iconS3Location
      `Prelude.hashWithSalt` iconURL
      `Prelude.hashWithSalt` instanceFamilies
      `Prelude.hashWithSalt` launchParameters
      `Prelude.hashWithSalt` launchPath
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` platforms
      `Prelude.hashWithSalt` workingDirectory

instance Prelude.NFData Application where
  rnf Application' {..} =
    Prelude.rnf appBlockArn
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf iconS3Location
      `Prelude.seq` Prelude.rnf iconURL
      `Prelude.seq` Prelude.rnf instanceFamilies
      `Prelude.seq` Prelude.rnf launchParameters
      `Prelude.seq` Prelude.rnf launchPath
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf platforms
      `Prelude.seq` Prelude.rnf workingDirectory
