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
-- Module      : Network.AWS.WorkSpaces.Types.RootStorage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.RootStorage where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the root volume for a WorkSpace bundle.
--
-- /See:/ 'newRootStorage' smart constructor.
data RootStorage = RootStorage'
  { -- | The size of the root volume.
    capacity :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON RootStorage where
  parseJSON =
    Prelude.withObject
      "RootStorage"
      ( \x ->
          RootStorage' Prelude.<$> (x Prelude..:? "Capacity")
      )

instance Prelude.Hashable RootStorage

instance Prelude.NFData RootStorage
