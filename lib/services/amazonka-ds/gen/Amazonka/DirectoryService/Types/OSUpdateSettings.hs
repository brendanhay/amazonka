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
-- Module      : Amazonka.DirectoryService.Types.OSUpdateSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.OSUpdateSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types.OSVersion
import qualified Amazonka.Prelude as Prelude

-- | OS version that the directory needs to be updated to.
--
-- /See:/ 'newOSUpdateSettings' smart constructor.
data OSUpdateSettings = OSUpdateSettings'
  { -- | OS version that the directory needs to be updated to.
    oSVersion :: Prelude.Maybe OSVersion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OSUpdateSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oSVersion', 'oSUpdateSettings_oSVersion' - OS version that the directory needs to be updated to.
newOSUpdateSettings ::
  OSUpdateSettings
newOSUpdateSettings =
  OSUpdateSettings' {oSVersion = Prelude.Nothing}

-- | OS version that the directory needs to be updated to.
oSUpdateSettings_oSVersion :: Lens.Lens' OSUpdateSettings (Prelude.Maybe OSVersion)
oSUpdateSettings_oSVersion = Lens.lens (\OSUpdateSettings' {oSVersion} -> oSVersion) (\s@OSUpdateSettings' {} a -> s {oSVersion = a} :: OSUpdateSettings)

instance Data.FromJSON OSUpdateSettings where
  parseJSON =
    Data.withObject
      "OSUpdateSettings"
      ( \x ->
          OSUpdateSettings'
            Prelude.<$> (x Data..:? "OSVersion")
      )

instance Prelude.Hashable OSUpdateSettings where
  hashWithSalt _salt OSUpdateSettings' {..} =
    _salt `Prelude.hashWithSalt` oSVersion

instance Prelude.NFData OSUpdateSettings where
  rnf OSUpdateSettings' {..} = Prelude.rnf oSVersion

instance Data.ToJSON OSUpdateSettings where
  toJSON OSUpdateSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [("OSVersion" Data..=) Prelude.<$> oSVersion]
      )
