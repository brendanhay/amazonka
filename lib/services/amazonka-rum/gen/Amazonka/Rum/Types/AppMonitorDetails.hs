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
-- Module      : Amazonka.Rum.Types.AppMonitorDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rum.Types.AppMonitorDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains information about the RUM app monitor.
--
-- /See:/ 'newAppMonitorDetails' smart constructor.
data AppMonitorDetails = AppMonitorDetails'
  { -- | The name of the app monitor.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the app monitor.
    id :: Prelude.Maybe Prelude.Text,
    -- | The version of the app monitor.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppMonitorDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'appMonitorDetails_name' - The name of the app monitor.
--
-- 'id', 'appMonitorDetails_id' - The unique ID of the app monitor.
--
-- 'version', 'appMonitorDetails_version' - The version of the app monitor.
newAppMonitorDetails ::
  AppMonitorDetails
newAppMonitorDetails =
  AppMonitorDetails'
    { name = Prelude.Nothing,
      id = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The name of the app monitor.
appMonitorDetails_name :: Lens.Lens' AppMonitorDetails (Prelude.Maybe Prelude.Text)
appMonitorDetails_name = Lens.lens (\AppMonitorDetails' {name} -> name) (\s@AppMonitorDetails' {} a -> s {name = a} :: AppMonitorDetails)

-- | The unique ID of the app monitor.
appMonitorDetails_id :: Lens.Lens' AppMonitorDetails (Prelude.Maybe Prelude.Text)
appMonitorDetails_id = Lens.lens (\AppMonitorDetails' {id} -> id) (\s@AppMonitorDetails' {} a -> s {id = a} :: AppMonitorDetails)

-- | The version of the app monitor.
appMonitorDetails_version :: Lens.Lens' AppMonitorDetails (Prelude.Maybe Prelude.Text)
appMonitorDetails_version = Lens.lens (\AppMonitorDetails' {version} -> version) (\s@AppMonitorDetails' {} a -> s {version = a} :: AppMonitorDetails)

instance Prelude.Hashable AppMonitorDetails where
  hashWithSalt _salt AppMonitorDetails' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` version

instance Prelude.NFData AppMonitorDetails where
  rnf AppMonitorDetails' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf version

instance Core.ToJSON AppMonitorDetails where
  toJSON AppMonitorDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("name" Core..=) Prelude.<$> name,
            ("id" Core..=) Prelude.<$> id,
            ("version" Core..=) Prelude.<$> version
          ]
      )
