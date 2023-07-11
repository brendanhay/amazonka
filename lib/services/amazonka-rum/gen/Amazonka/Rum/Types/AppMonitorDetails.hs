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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rum.Types.AppMonitorDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains information about the RUM app monitor.
--
-- /See:/ 'newAppMonitorDetails' smart constructor.
data AppMonitorDetails = AppMonitorDetails'
  { -- | The unique ID of the app monitor.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the app monitor.
    name :: Prelude.Maybe Prelude.Text,
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
-- 'id', 'appMonitorDetails_id' - The unique ID of the app monitor.
--
-- 'name', 'appMonitorDetails_name' - The name of the app monitor.
--
-- 'version', 'appMonitorDetails_version' - The version of the app monitor.
newAppMonitorDetails ::
  AppMonitorDetails
newAppMonitorDetails =
  AppMonitorDetails'
    { id = Prelude.Nothing,
      name = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The unique ID of the app monitor.
appMonitorDetails_id :: Lens.Lens' AppMonitorDetails (Prelude.Maybe Prelude.Text)
appMonitorDetails_id = Lens.lens (\AppMonitorDetails' {id} -> id) (\s@AppMonitorDetails' {} a -> s {id = a} :: AppMonitorDetails)

-- | The name of the app monitor.
appMonitorDetails_name :: Lens.Lens' AppMonitorDetails (Prelude.Maybe Prelude.Text)
appMonitorDetails_name = Lens.lens (\AppMonitorDetails' {name} -> name) (\s@AppMonitorDetails' {} a -> s {name = a} :: AppMonitorDetails)

-- | The version of the app monitor.
appMonitorDetails_version :: Lens.Lens' AppMonitorDetails (Prelude.Maybe Prelude.Text)
appMonitorDetails_version = Lens.lens (\AppMonitorDetails' {version} -> version) (\s@AppMonitorDetails' {} a -> s {version = a} :: AppMonitorDetails)

instance Prelude.Hashable AppMonitorDetails where
  hashWithSalt _salt AppMonitorDetails' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` version

instance Prelude.NFData AppMonitorDetails where
  rnf AppMonitorDetails' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf version

instance Data.ToJSON AppMonitorDetails where
  toJSON AppMonitorDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("id" Data..=) Prelude.<$> id,
            ("name" Data..=) Prelude.<$> name,
            ("version" Data..=) Prelude.<$> version
          ]
      )
