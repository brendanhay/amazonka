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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.Application where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an application in the application catalog.
--
-- /See:/ 'newApplication' smart constructor.
data Application = Application'
  { -- | If there is a problem, the application can be disabled after image
    -- creation.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The path to the application executable in the instance.
    launchPath :: Prelude.Maybe Prelude.Text,
    -- | The arguments that are passed to the application at launch.
    launchParameters :: Prelude.Maybe Prelude.Text,
    -- | The name of the application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The application name to display.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | Additional attributes that describe the application.
    metadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The URL for the application icon. This URL might be time-limited.
    iconURL :: Prelude.Maybe Prelude.Text
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
-- 'enabled', 'application_enabled' - If there is a problem, the application can be disabled after image
-- creation.
--
-- 'launchPath', 'application_launchPath' - The path to the application executable in the instance.
--
-- 'launchParameters', 'application_launchParameters' - The arguments that are passed to the application at launch.
--
-- 'name', 'application_name' - The name of the application.
--
-- 'displayName', 'application_displayName' - The application name to display.
--
-- 'metadata', 'application_metadata' - Additional attributes that describe the application.
--
-- 'iconURL', 'application_iconURL' - The URL for the application icon. This URL might be time-limited.
newApplication ::
  Application
newApplication =
  Application'
    { enabled = Prelude.Nothing,
      launchPath = Prelude.Nothing,
      launchParameters = Prelude.Nothing,
      name = Prelude.Nothing,
      displayName = Prelude.Nothing,
      metadata = Prelude.Nothing,
      iconURL = Prelude.Nothing
    }

-- | If there is a problem, the application can be disabled after image
-- creation.
application_enabled :: Lens.Lens' Application (Prelude.Maybe Prelude.Bool)
application_enabled = Lens.lens (\Application' {enabled} -> enabled) (\s@Application' {} a -> s {enabled = a} :: Application)

-- | The path to the application executable in the instance.
application_launchPath :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_launchPath = Lens.lens (\Application' {launchPath} -> launchPath) (\s@Application' {} a -> s {launchPath = a} :: Application)

-- | The arguments that are passed to the application at launch.
application_launchParameters :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_launchParameters = Lens.lens (\Application' {launchParameters} -> launchParameters) (\s@Application' {} a -> s {launchParameters = a} :: Application)

-- | The name of the application.
application_name :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_name = Lens.lens (\Application' {name} -> name) (\s@Application' {} a -> s {name = a} :: Application)

-- | The application name to display.
application_displayName :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_displayName = Lens.lens (\Application' {displayName} -> displayName) (\s@Application' {} a -> s {displayName = a} :: Application)

-- | Additional attributes that describe the application.
application_metadata :: Lens.Lens' Application (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
application_metadata = Lens.lens (\Application' {metadata} -> metadata) (\s@Application' {} a -> s {metadata = a} :: Application) Prelude.. Lens.mapping Lens.coerced

-- | The URL for the application icon. This URL might be time-limited.
application_iconURL :: Lens.Lens' Application (Prelude.Maybe Prelude.Text)
application_iconURL = Lens.lens (\Application' {iconURL} -> iconURL) (\s@Application' {} a -> s {iconURL = a} :: Application)

instance Core.FromJSON Application where
  parseJSON =
    Core.withObject
      "Application"
      ( \x ->
          Application'
            Prelude.<$> (x Core..:? "Enabled")
            Prelude.<*> (x Core..:? "LaunchPath")
            Prelude.<*> (x Core..:? "LaunchParameters")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "DisplayName")
            Prelude.<*> (x Core..:? "Metadata" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "IconURL")
      )

instance Prelude.Hashable Application where
  hashWithSalt salt' Application' {..} =
    salt' `Prelude.hashWithSalt` iconURL
      `Prelude.hashWithSalt` metadata
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` launchParameters
      `Prelude.hashWithSalt` launchPath
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData Application where
  rnf Application' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf iconURL
      `Prelude.seq` Prelude.rnf metadata
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf launchParameters
      `Prelude.seq` Prelude.rnf launchPath
