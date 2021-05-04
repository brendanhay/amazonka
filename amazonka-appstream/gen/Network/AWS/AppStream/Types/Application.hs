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
-- Module      : Network.AWS.AppStream.Types.Application
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.Application where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an application in the application catalog.
--
-- /See:/ 'newApplication' smart constructor.
data Application = Application'
  { -- | The URL for the application icon. This URL might be time-limited.
    iconURL :: Prelude.Maybe Prelude.Text,
    -- | The path to the application executable in the instance.
    launchPath :: Prelude.Maybe Prelude.Text,
    -- | If there is a problem, the application can be disabled after image
    -- creation.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Additional attributes that describe the application.
    metadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The arguments that are passed to the application at launch.
    launchParameters :: Prelude.Maybe Prelude.Text,
    -- | The name of the application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The application name to display.
    displayName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Application' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iconURL', 'application_iconURL' - The URL for the application icon. This URL might be time-limited.
--
-- 'launchPath', 'application_launchPath' - The path to the application executable in the instance.
--
-- 'enabled', 'application_enabled' - If there is a problem, the application can be disabled after image
-- creation.
--
-- 'metadata', 'application_metadata' - Additional attributes that describe the application.
--
-- 'launchParameters', 'application_launchParameters' - The arguments that are passed to the application at launch.
--
-- 'name', 'application_name' - The name of the application.
--
-- 'displayName', 'application_displayName' - The application name to display.
newApplication ::
  Application
newApplication =
  Application'
    { iconURL = Prelude.Nothing,
      launchPath = Prelude.Nothing,
      enabled = Prelude.Nothing,
      metadata = Prelude.Nothing,
      launchParameters = Prelude.Nothing,
      name = Prelude.Nothing,
      displayName = Prelude.Nothing
    }

-- | The URL for the application icon. This URL might be time-limited.
application_iconURL :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_iconURL = Lens.lens (\Application' {iconURL} -> iconURL) (\s@Application' {} a -> s {iconURL = a} :: Application)

-- | The path to the application executable in the instance.
application_launchPath :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_launchPath = Lens.lens (\Application' {launchPath} -> launchPath) (\s@Application' {} a -> s {launchPath = a} :: Application)

-- | If there is a problem, the application can be disabled after image
-- creation.
application_enabled :: Lens.Lens' Application (Prelude.Maybe Prelude.Bool)
application_enabled = Lens.lens (\Application' {enabled} -> enabled) (\s@Application' {} a -> s {enabled = a} :: Application)

-- | Additional attributes that describe the application.
application_metadata :: Lens.Lens' Application (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
application_metadata = Lens.lens (\Application' {metadata} -> metadata) (\s@Application' {} a -> s {metadata = a} :: Application) Prelude.. Lens.mapping Prelude._Coerce

-- | The arguments that are passed to the application at launch.
application_launchParameters :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_launchParameters = Lens.lens (\Application' {launchParameters} -> launchParameters) (\s@Application' {} a -> s {launchParameters = a} :: Application)

-- | The name of the application.
application_name :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_name = Lens.lens (\Application' {name} -> name) (\s@Application' {} a -> s {name = a} :: Application)

-- | The application name to display.
application_displayName :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_displayName = Lens.lens (\Application' {displayName} -> displayName) (\s@Application' {} a -> s {displayName = a} :: Application)

instance Prelude.FromJSON Application where
  parseJSON =
    Prelude.withObject
      "Application"
      ( \x ->
          Application'
            Prelude.<$> (x Prelude..:? "IconURL")
            Prelude.<*> (x Prelude..:? "LaunchPath")
            Prelude.<*> (x Prelude..:? "Enabled")
            Prelude.<*> (x Prelude..:? "Metadata" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "LaunchParameters")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "DisplayName")
      )

instance Prelude.Hashable Application

instance Prelude.NFData Application
