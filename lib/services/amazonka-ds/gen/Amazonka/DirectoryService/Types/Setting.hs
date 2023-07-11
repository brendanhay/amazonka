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
-- Module      : Amazonka.DirectoryService.Types.Setting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.Setting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the configurable settings for a directory.
--
-- /See:/ 'newSetting' smart constructor.
data Setting = Setting'
  { -- | The name of the directory setting. For example:
    --
    -- @TLS_1_0@
    name :: Prelude.Text,
    -- | The value of the directory setting for which to retrieve information.
    -- For example, for @TLS_1_0@, the valid values are: @Enable@ and
    -- @Disable@.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Setting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'setting_name' - The name of the directory setting. For example:
--
-- @TLS_1_0@
--
-- 'value', 'setting_value' - The value of the directory setting for which to retrieve information.
-- For example, for @TLS_1_0@, the valid values are: @Enable@ and
-- @Disable@.
newSetting ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  Setting
newSetting pName_ pValue_ =
  Setting' {name = pName_, value = pValue_}

-- | The name of the directory setting. For example:
--
-- @TLS_1_0@
setting_name :: Lens.Lens' Setting Prelude.Text
setting_name = Lens.lens (\Setting' {name} -> name) (\s@Setting' {} a -> s {name = a} :: Setting)

-- | The value of the directory setting for which to retrieve information.
-- For example, for @TLS_1_0@, the valid values are: @Enable@ and
-- @Disable@.
setting_value :: Lens.Lens' Setting Prelude.Text
setting_value = Lens.lens (\Setting' {value} -> value) (\s@Setting' {} a -> s {value = a} :: Setting)

instance Prelude.Hashable Setting where
  hashWithSalt _salt Setting' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData Setting where
  rnf Setting' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON Setting where
  toJSON Setting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Value" Data..= value)
          ]
      )
