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
-- Module      : Network.AWS.Shield.Types.Limit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.Limit where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies how many protections of a given type you can create.
--
-- /See:/ 'newLimit' smart constructor.
data Limit = Limit'
  { -- | The maximum number of protections that can be created for the specified
    -- @Type@.
    max :: Core.Maybe Core.Integer,
    -- | The type of protection.
    type' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Limit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'limit_max' - The maximum number of protections that can be created for the specified
-- @Type@.
--
-- 'type'', 'limit_type' - The type of protection.
newLimit ::
  Limit
newLimit =
  Limit' {max = Core.Nothing, type' = Core.Nothing}

-- | The maximum number of protections that can be created for the specified
-- @Type@.
limit_max :: Lens.Lens' Limit (Core.Maybe Core.Integer)
limit_max = Lens.lens (\Limit' {max} -> max) (\s@Limit' {} a -> s {max = a} :: Limit)

-- | The type of protection.
limit_type :: Lens.Lens' Limit (Core.Maybe Core.Text)
limit_type = Lens.lens (\Limit' {type'} -> type') (\s@Limit' {} a -> s {type' = a} :: Limit)

instance Core.FromJSON Limit where
  parseJSON =
    Core.withObject
      "Limit"
      ( \x ->
          Limit'
            Core.<$> (x Core..:? "Max") Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable Limit

instance Core.NFData Limit
