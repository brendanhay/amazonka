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
-- Module      : Amazonka.WorkSpaces.Types.RootStorage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.RootStorage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the root volume for a WorkSpace bundle.
--
-- /See:/ 'newRootStorage' smart constructor.
data RootStorage = RootStorage'
  { -- | The size of the root volume.
    capacity :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RootStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacity', 'rootStorage_capacity' - The size of the root volume.
newRootStorage ::
  RootStorage
newRootStorage =
  RootStorage' {capacity = Prelude.Nothing}

-- | The size of the root volume.
rootStorage_capacity :: Lens.Lens' RootStorage (Prelude.Maybe Prelude.Text)
rootStorage_capacity = Lens.lens (\RootStorage' {capacity} -> capacity) (\s@RootStorage' {} a -> s {capacity = a} :: RootStorage)

instance Data.FromJSON RootStorage where
  parseJSON =
    Data.withObject
      "RootStorage"
      ( \x ->
          RootStorage' Prelude.<$> (x Data..:? "Capacity")
      )

instance Prelude.Hashable RootStorage where
  hashWithSalt _salt RootStorage' {..} =
    _salt `Prelude.hashWithSalt` capacity

instance Prelude.NFData RootStorage where
  rnf RootStorage' {..} = Prelude.rnf capacity

instance Data.ToJSON RootStorage where
  toJSON RootStorage' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Capacity" Data..=) Prelude.<$> capacity]
      )
