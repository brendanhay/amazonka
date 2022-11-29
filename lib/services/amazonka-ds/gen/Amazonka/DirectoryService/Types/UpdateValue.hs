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
-- Module      : Amazonka.DirectoryService.Types.UpdateValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.UpdateValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectoryService.Types.OSUpdateSettings
import qualified Amazonka.Prelude as Prelude

-- | The value for a given type of @UpdateSettings@.
--
-- /See:/ 'newUpdateValue' smart constructor.
data UpdateValue = UpdateValue'
  { -- | The OS update related settings.
    oSUpdateSettings :: Prelude.Maybe OSUpdateSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oSUpdateSettings', 'updateValue_oSUpdateSettings' - The OS update related settings.
newUpdateValue ::
  UpdateValue
newUpdateValue =
  UpdateValue' {oSUpdateSettings = Prelude.Nothing}

-- | The OS update related settings.
updateValue_oSUpdateSettings :: Lens.Lens' UpdateValue (Prelude.Maybe OSUpdateSettings)
updateValue_oSUpdateSettings = Lens.lens (\UpdateValue' {oSUpdateSettings} -> oSUpdateSettings) (\s@UpdateValue' {} a -> s {oSUpdateSettings = a} :: UpdateValue)

instance Core.FromJSON UpdateValue where
  parseJSON =
    Core.withObject
      "UpdateValue"
      ( \x ->
          UpdateValue'
            Prelude.<$> (x Core..:? "OSUpdateSettings")
      )

instance Prelude.Hashable UpdateValue where
  hashWithSalt _salt UpdateValue' {..} =
    _salt `Prelude.hashWithSalt` oSUpdateSettings

instance Prelude.NFData UpdateValue where
  rnf UpdateValue' {..} = Prelude.rnf oSUpdateSettings
