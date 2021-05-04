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
-- Module      : Network.AWS.Athena.Types.EngineVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.EngineVersion where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The Athena engine version for running queries.
--
-- /See:/ 'newEngineVersion' smart constructor.
data EngineVersion = EngineVersion'
  { -- | Read only. The engine version on which the query runs. If the user
    -- requests a valid engine version other than Auto, the effective engine
    -- version is the same as the engine version that the user requested. If
    -- the user requests Auto, the effective engine version is chosen by
    -- Athena. When a request to update the engine version is made by a
    -- @CreateWorkGroup@ or @UpdateWorkGroup@ operation, the
    -- @EffectiveEngineVersion@ field is ignored.
    effectiveEngineVersion :: Prelude.Maybe Prelude.Text,
    -- | The engine version requested by the user. Possible values are determined
    -- by the output of @ListEngineVersions@, including Auto. The default is
    -- Auto.
    selectedEngineVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EngineVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'effectiveEngineVersion', 'engineVersion_effectiveEngineVersion' - Read only. The engine version on which the query runs. If the user
-- requests a valid engine version other than Auto, the effective engine
-- version is the same as the engine version that the user requested. If
-- the user requests Auto, the effective engine version is chosen by
-- Athena. When a request to update the engine version is made by a
-- @CreateWorkGroup@ or @UpdateWorkGroup@ operation, the
-- @EffectiveEngineVersion@ field is ignored.
--
-- 'selectedEngineVersion', 'engineVersion_selectedEngineVersion' - The engine version requested by the user. Possible values are determined
-- by the output of @ListEngineVersions@, including Auto. The default is
-- Auto.
newEngineVersion ::
  EngineVersion
newEngineVersion =
  EngineVersion'
    { effectiveEngineVersion =
        Prelude.Nothing,
      selectedEngineVersion = Prelude.Nothing
    }

-- | Read only. The engine version on which the query runs. If the user
-- requests a valid engine version other than Auto, the effective engine
-- version is the same as the engine version that the user requested. If
-- the user requests Auto, the effective engine version is chosen by
-- Athena. When a request to update the engine version is made by a
-- @CreateWorkGroup@ or @UpdateWorkGroup@ operation, the
-- @EffectiveEngineVersion@ field is ignored.
engineVersion_effectiveEngineVersion :: Lens.Lens' EngineVersion (Prelude.Maybe Prelude.Text)
engineVersion_effectiveEngineVersion = Lens.lens (\EngineVersion' {effectiveEngineVersion} -> effectiveEngineVersion) (\s@EngineVersion' {} a -> s {effectiveEngineVersion = a} :: EngineVersion)

-- | The engine version requested by the user. Possible values are determined
-- by the output of @ListEngineVersions@, including Auto. The default is
-- Auto.
engineVersion_selectedEngineVersion :: Lens.Lens' EngineVersion (Prelude.Maybe Prelude.Text)
engineVersion_selectedEngineVersion = Lens.lens (\EngineVersion' {selectedEngineVersion} -> selectedEngineVersion) (\s@EngineVersion' {} a -> s {selectedEngineVersion = a} :: EngineVersion)

instance Prelude.FromJSON EngineVersion where
  parseJSON =
    Prelude.withObject
      "EngineVersion"
      ( \x ->
          EngineVersion'
            Prelude.<$> (x Prelude..:? "EffectiveEngineVersion")
            Prelude.<*> (x Prelude..:? "SelectedEngineVersion")
      )

instance Prelude.Hashable EngineVersion

instance Prelude.NFData EngineVersion

instance Prelude.ToJSON EngineVersion where
  toJSON EngineVersion' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EffectiveEngineVersion" Prelude..=)
              Prelude.<$> effectiveEngineVersion,
            ("SelectedEngineVersion" Prelude..=)
              Prelude.<$> selectedEngineVersion
          ]
      )
