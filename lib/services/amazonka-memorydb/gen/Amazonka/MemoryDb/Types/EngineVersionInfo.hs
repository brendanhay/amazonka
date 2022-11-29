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
-- Module      : Amazonka.MemoryDb.Types.EngineVersionInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types.EngineVersionInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides details of the Redis engine version
--
-- /See:/ 'newEngineVersionInfo' smart constructor.
data EngineVersionInfo = EngineVersionInfo'
  { -- | Specifies the name of the parameter group family to which the engine
    -- default parameters apply.
    parameterGroupFamily :: Prelude.Maybe Prelude.Text,
    -- | The patched engine version
    enginePatchVersion :: Prelude.Maybe Prelude.Text,
    -- | The engine version
    engineVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EngineVersionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterGroupFamily', 'engineVersionInfo_parameterGroupFamily' - Specifies the name of the parameter group family to which the engine
-- default parameters apply.
--
-- 'enginePatchVersion', 'engineVersionInfo_enginePatchVersion' - The patched engine version
--
-- 'engineVersion', 'engineVersionInfo_engineVersion' - The engine version
newEngineVersionInfo ::
  EngineVersionInfo
newEngineVersionInfo =
  EngineVersionInfo'
    { parameterGroupFamily =
        Prelude.Nothing,
      enginePatchVersion = Prelude.Nothing,
      engineVersion = Prelude.Nothing
    }

-- | Specifies the name of the parameter group family to which the engine
-- default parameters apply.
engineVersionInfo_parameterGroupFamily :: Lens.Lens' EngineVersionInfo (Prelude.Maybe Prelude.Text)
engineVersionInfo_parameterGroupFamily = Lens.lens (\EngineVersionInfo' {parameterGroupFamily} -> parameterGroupFamily) (\s@EngineVersionInfo' {} a -> s {parameterGroupFamily = a} :: EngineVersionInfo)

-- | The patched engine version
engineVersionInfo_enginePatchVersion :: Lens.Lens' EngineVersionInfo (Prelude.Maybe Prelude.Text)
engineVersionInfo_enginePatchVersion = Lens.lens (\EngineVersionInfo' {enginePatchVersion} -> enginePatchVersion) (\s@EngineVersionInfo' {} a -> s {enginePatchVersion = a} :: EngineVersionInfo)

-- | The engine version
engineVersionInfo_engineVersion :: Lens.Lens' EngineVersionInfo (Prelude.Maybe Prelude.Text)
engineVersionInfo_engineVersion = Lens.lens (\EngineVersionInfo' {engineVersion} -> engineVersion) (\s@EngineVersionInfo' {} a -> s {engineVersion = a} :: EngineVersionInfo)

instance Core.FromJSON EngineVersionInfo where
  parseJSON =
    Core.withObject
      "EngineVersionInfo"
      ( \x ->
          EngineVersionInfo'
            Prelude.<$> (x Core..:? "ParameterGroupFamily")
            Prelude.<*> (x Core..:? "EnginePatchVersion")
            Prelude.<*> (x Core..:? "EngineVersion")
      )

instance Prelude.Hashable EngineVersionInfo where
  hashWithSalt _salt EngineVersionInfo' {..} =
    _salt `Prelude.hashWithSalt` parameterGroupFamily
      `Prelude.hashWithSalt` enginePatchVersion
      `Prelude.hashWithSalt` engineVersion

instance Prelude.NFData EngineVersionInfo where
  rnf EngineVersionInfo' {..} =
    Prelude.rnf parameterGroupFamily
      `Prelude.seq` Prelude.rnf enginePatchVersion
      `Prelude.seq` Prelude.rnf engineVersion
