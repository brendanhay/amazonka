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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the root volume for a WorkSpace bundle.
--
-- /See:/ 'newRootStorage' smart constructor.
data RootStorage = RootStorage'
  { -- | The size of the root volume.
    capacity :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  RootStorage' {capacity = Core.Nothing}

-- | The size of the root volume.
rootStorage_capacity :: Lens.Lens' RootStorage (Core.Maybe Core.Text)
rootStorage_capacity = Lens.lens (\RootStorage' {capacity} -> capacity) (\s@RootStorage' {} a -> s {capacity = a} :: RootStorage)

instance Core.FromJSON RootStorage where
  parseJSON =
    Core.withObject
      "RootStorage"
      ( \x ->
          RootStorage' Core.<$> (x Core..:? "Capacity")
      )

instance Core.Hashable RootStorage

instance Core.NFData RootStorage
