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
-- Module      : Network.AWS.ECS.Types.Ulimit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Ulimit where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types.UlimitName
import qualified Network.AWS.Lens as Lens

-- | The @ulimit@ settings to pass to the container.
--
-- /See:/ 'newUlimit' smart constructor.
data Ulimit = Ulimit'
  { -- | The @type@ of the @ulimit@.
    name :: UlimitName,
    -- | The soft limit for the ulimit type.
    softLimit :: Core.Int,
    -- | The hard limit for the ulimit type.
    hardLimit :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Ulimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'ulimit_name' - The @type@ of the @ulimit@.
--
-- 'softLimit', 'ulimit_softLimit' - The soft limit for the ulimit type.
--
-- 'hardLimit', 'ulimit_hardLimit' - The hard limit for the ulimit type.
newUlimit ::
  -- | 'name'
  UlimitName ->
  -- | 'softLimit'
  Core.Int ->
  -- | 'hardLimit'
  Core.Int ->
  Ulimit
newUlimit pName_ pSoftLimit_ pHardLimit_ =
  Ulimit'
    { name = pName_,
      softLimit = pSoftLimit_,
      hardLimit = pHardLimit_
    }

-- | The @type@ of the @ulimit@.
ulimit_name :: Lens.Lens' Ulimit UlimitName
ulimit_name = Lens.lens (\Ulimit' {name} -> name) (\s@Ulimit' {} a -> s {name = a} :: Ulimit)

-- | The soft limit for the ulimit type.
ulimit_softLimit :: Lens.Lens' Ulimit Core.Int
ulimit_softLimit = Lens.lens (\Ulimit' {softLimit} -> softLimit) (\s@Ulimit' {} a -> s {softLimit = a} :: Ulimit)

-- | The hard limit for the ulimit type.
ulimit_hardLimit :: Lens.Lens' Ulimit Core.Int
ulimit_hardLimit = Lens.lens (\Ulimit' {hardLimit} -> hardLimit) (\s@Ulimit' {} a -> s {hardLimit = a} :: Ulimit)

instance Core.FromJSON Ulimit where
  parseJSON =
    Core.withObject
      "Ulimit"
      ( \x ->
          Ulimit'
            Core.<$> (x Core..: "name")
            Core.<*> (x Core..: "softLimit")
            Core.<*> (x Core..: "hardLimit")
      )

instance Core.Hashable Ulimit

instance Core.NFData Ulimit

instance Core.ToJSON Ulimit where
  toJSON Ulimit' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("softLimit" Core..= softLimit),
            Core.Just ("hardLimit" Core..= hardLimit)
          ]
      )
