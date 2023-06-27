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
-- Module      : Amazonka.ServiceCatalog.Types.LaunchPath
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.LaunchPath where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A launch path object.
--
-- /See:/ 'newLaunchPath' smart constructor.
data LaunchPath = LaunchPath'
  { -- | The identifier of the launch path.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch path.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchPath' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'launchPath_id' - The identifier of the launch path.
--
-- 'name', 'launchPath_name' - The name of the launch path.
newLaunchPath ::
  LaunchPath
newLaunchPath =
  LaunchPath'
    { id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The identifier of the launch path.
launchPath_id :: Lens.Lens' LaunchPath (Prelude.Maybe Prelude.Text)
launchPath_id = Lens.lens (\LaunchPath' {id} -> id) (\s@LaunchPath' {} a -> s {id = a} :: LaunchPath)

-- | The name of the launch path.
launchPath_name :: Lens.Lens' LaunchPath (Prelude.Maybe Prelude.Text)
launchPath_name = Lens.lens (\LaunchPath' {name} -> name) (\s@LaunchPath' {} a -> s {name = a} :: LaunchPath)

instance Data.FromJSON LaunchPath where
  parseJSON =
    Data.withObject
      "LaunchPath"
      ( \x ->
          LaunchPath'
            Prelude.<$> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable LaunchPath where
  hashWithSalt _salt LaunchPath' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name

instance Prelude.NFData LaunchPath where
  rnf LaunchPath' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf name
