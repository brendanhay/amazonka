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
-- Module      : Network.AWS.EMR.Types.Application
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Application where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | With Amazon EMR release version 4.0 and later, the only accepted
-- parameter is the application name. To pass arguments to applications,
-- you use configuration classifications specified using configuration JSON
-- objects. For more information, see
-- <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/emr-configure-apps.html Configuring Applications>.
--
-- With earlier Amazon EMR releases, the application is any Amazon or
-- third-party software that you can add to the cluster. This structure
-- contains a list of strings that indicates the software to use with the
-- cluster and accepts a user argument list. Amazon EMR accepts and
-- forwards the argument list to the corresponding installation script as
-- bootstrap action argument.
--
-- /See:/ 'newApplication' smart constructor.
data Application = Application'
  { -- | Arguments for Amazon EMR to pass to the application.
    args :: Core.Maybe [Core.Text],
    -- | This option is for advanced users only. This is meta information about
    -- third-party applications that third-party vendors use for testing
    -- purposes.
    additionalInfo :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The version of the application.
    version :: Core.Maybe Core.Text,
    -- | The name of the application.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Application' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'args', 'application_args' - Arguments for Amazon EMR to pass to the application.
--
-- 'additionalInfo', 'application_additionalInfo' - This option is for advanced users only. This is meta information about
-- third-party applications that third-party vendors use for testing
-- purposes.
--
-- 'version', 'application_version' - The version of the application.
--
-- 'name', 'application_name' - The name of the application.
newApplication ::
  Application
newApplication =
  Application'
    { args = Core.Nothing,
      additionalInfo = Core.Nothing,
      version = Core.Nothing,
      name = Core.Nothing
    }

-- | Arguments for Amazon EMR to pass to the application.
application_args :: Lens.Lens' Application (Core.Maybe [Core.Text])
application_args = Lens.lens (\Application' {args} -> args) (\s@Application' {} a -> s {args = a} :: Application) Core.. Lens.mapping Lens._Coerce

-- | This option is for advanced users only. This is meta information about
-- third-party applications that third-party vendors use for testing
-- purposes.
application_additionalInfo :: Lens.Lens' Application (Core.Maybe (Core.HashMap Core.Text Core.Text))
application_additionalInfo = Lens.lens (\Application' {additionalInfo} -> additionalInfo) (\s@Application' {} a -> s {additionalInfo = a} :: Application) Core.. Lens.mapping Lens._Coerce

-- | The version of the application.
application_version :: Lens.Lens' Application (Core.Maybe Core.Text)
application_version = Lens.lens (\Application' {version} -> version) (\s@Application' {} a -> s {version = a} :: Application)

-- | The name of the application.
application_name :: Lens.Lens' Application (Core.Maybe Core.Text)
application_name = Lens.lens (\Application' {name} -> name) (\s@Application' {} a -> s {name = a} :: Application)

instance Core.FromJSON Application where
  parseJSON =
    Core.withObject
      "Application"
      ( \x ->
          Application'
            Core.<$> (x Core..:? "Args" Core..!= Core.mempty)
            Core.<*> (x Core..:? "AdditionalInfo" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "Name")
      )

instance Core.Hashable Application

instance Core.NFData Application

instance Core.ToJSON Application where
  toJSON Application' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Args" Core..=) Core.<$> args,
            ("AdditionalInfo" Core..=) Core.<$> additionalInfo,
            ("Version" Core..=) Core.<$> version,
            ("Name" Core..=) Core.<$> name
          ]
      )
