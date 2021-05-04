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
-- Module      : Network.AWS.MQ.Types.EngineVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.EngineVersion where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Id of the engine version.
--
-- /See:/ 'newEngineVersion' smart constructor.
data EngineVersion = EngineVersion'
  { -- | Id for the version.
    name :: Prelude.Maybe Prelude.Text
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
-- 'name', 'engineVersion_name' - Id for the version.
newEngineVersion ::
  EngineVersion
newEngineVersion =
  EngineVersion' {name = Prelude.Nothing}

-- | Id for the version.
engineVersion_name :: Lens.Lens' EngineVersion (Prelude.Maybe Prelude.Text)
engineVersion_name = Lens.lens (\EngineVersion' {name} -> name) (\s@EngineVersion' {} a -> s {name = a} :: EngineVersion)

instance Prelude.FromJSON EngineVersion where
  parseJSON =
    Prelude.withObject
      "EngineVersion"
      ( \x ->
          EngineVersion' Prelude.<$> (x Prelude..:? "name")
      )

instance Prelude.Hashable EngineVersion

instance Prelude.NFData EngineVersion
