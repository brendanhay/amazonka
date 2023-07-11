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
-- Module      : Amazonka.MQ.Types.EngineVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.EngineVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Id of the engine version.
--
-- /See:/ 'newEngineVersion' smart constructor.
data EngineVersion = EngineVersion'
  { -- | Id for the version.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EngineVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'engineVersion_name' - Id for the version.
newEngineVersion ::
  EngineVersion
newEngineVersion =
  EngineVersion' {name = Prelude.Nothing}

-- | Id for the version.
engineVersion_name :: Lens.Lens' EngineVersion (Prelude.Maybe Prelude.Text)
engineVersion_name = Lens.lens (\EngineVersion' {name} -> name) (\s@EngineVersion' {} a -> s {name = a} :: EngineVersion)

instance Data.FromJSON EngineVersion where
  parseJSON =
    Data.withObject
      "EngineVersion"
      ( \x ->
          EngineVersion' Prelude.<$> (x Data..:? "name")
      )

instance Prelude.Hashable EngineVersion where
  hashWithSalt _salt EngineVersion' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData EngineVersion where
  rnf EngineVersion' {..} = Prelude.rnf name
