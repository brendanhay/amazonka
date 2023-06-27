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
-- Module      : Amazonka.GroundStation.Types.ComponentVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.ComponentVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Version information for agent components.
--
-- /See:/ 'newComponentVersion' smart constructor.
data ComponentVersion = ComponentVersion'
  { -- | Component type.
    componentType :: Prelude.Text,
    -- | List of versions.
    versions :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentType', 'componentVersion_componentType' - Component type.
--
-- 'versions', 'componentVersion_versions' - List of versions.
newComponentVersion ::
  -- | 'componentType'
  Prelude.Text ->
  -- | 'versions'
  Prelude.NonEmpty Prelude.Text ->
  ComponentVersion
newComponentVersion pComponentType_ pVersions_ =
  ComponentVersion'
    { componentType = pComponentType_,
      versions = Lens.coerced Lens.# pVersions_
    }

-- | Component type.
componentVersion_componentType :: Lens.Lens' ComponentVersion Prelude.Text
componentVersion_componentType = Lens.lens (\ComponentVersion' {componentType} -> componentType) (\s@ComponentVersion' {} a -> s {componentType = a} :: ComponentVersion)

-- | List of versions.
componentVersion_versions :: Lens.Lens' ComponentVersion (Prelude.NonEmpty Prelude.Text)
componentVersion_versions = Lens.lens (\ComponentVersion' {versions} -> versions) (\s@ComponentVersion' {} a -> s {versions = a} :: ComponentVersion) Prelude.. Lens.coerced

instance Prelude.Hashable ComponentVersion where
  hashWithSalt _salt ComponentVersion' {..} =
    _salt
      `Prelude.hashWithSalt` componentType
      `Prelude.hashWithSalt` versions

instance Prelude.NFData ComponentVersion where
  rnf ComponentVersion' {..} =
    Prelude.rnf componentType
      `Prelude.seq` Prelude.rnf versions

instance Data.ToJSON ComponentVersion where
  toJSON ComponentVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("componentType" Data..= componentType),
            Prelude.Just ("versions" Data..= versions)
          ]
      )
